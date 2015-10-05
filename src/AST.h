#ifndef ICARUS_AST_H
#define ICARUS_AST_H

#include <string>
#include <iostream>
#include <map>
#include <set>
#include <vector>

#include "Language.h"
#include "Type.h"
#include "typedefs.h"

namespace AST {
  class Scope;
  class Declaration;

  class Node {
    public:
      static inline Node eof_node() {
        return Node(Language::eof, "");
      }
      static inline Node newline_node() {
        return Node(Language::newline, "");
      }
      static inline Node string_literal_node(const std::string& str_lit) { 
        return Node(Language::string_literal, str_lit);
      }

      Language::NodeType node_type() const { return type_; }
      void set_node_type(Language::NodeType t) { type_ = t; }

      std::string token() const { return token_; }
      void set_token(const std::string& token_string) {
        token_ = token_string;
      }

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope*) {}
      virtual void verify_types() {}
      virtual void find_all_decls(Scope*) {}

      virtual bool is_identifier() const { return type_ == Language::identifier; }
      virtual bool is_binop() const { return false; }

      Node(Language::NodeType type = Language::unknown,
          const std::string& token = "") : type_(type), token_(token) {}

      virtual ~Node(){}


      inline friend std::ostream& operator<<(std::ostream& os, const Node& node) {
        return os << node.to_string(0);
      }

    protected:
      Language::NodeType type_;
      std::string token_;
  };


  class Expression : public Node {
    friend class KVPairList;
    friend class Binop;
    friend class Declaration;
    friend class Assignment;
    friend class Scope;

    public:
    static NPtr parenthesize(NPtrVec&& nodes);

    size_t precedence() const { return precedence_; }

    virtual void join_identifiers(Scope* scope) = 0;
    virtual void verify_types() = 0;
    virtual void verify_type_is(Type t);

    virtual void find_all_decls(Scope*) {}

    virtual ~Expression(){}

    protected:
    Expression() {}

    size_t precedence_;
    Type expr_type_;
  };

  inline NPtr Expression::parenthesize(NPtrVec&& nodes) {
    auto expr_ptr = static_cast<Expression*>(nodes[1].release());
    expr_ptr->precedence_ = Language::op_prec.at("MAX");
    return NPtr(expr_ptr);
  }


  inline void Expression::verify_type_is(Type t) {
    verify_types();

    if (expr_type_ != t) {
      // TODO: give some context for this error message. Why must this be the type?
      // So far the only instance where this is called is for case statements,
      std::cerr << "Type of `____` must be " << t.to_string() << ", but " << expr_type_.to_string() << " found instead." << std::endl;
      expr_type_ = t;
    }
  }



  class Scope {
    public:
      static std::vector<Scope*> all_scopes;

      virtual std::string to_string(size_t n) const = 0;

      void verify_scope();

      void show_identifiers() const;
      void register_declaration(Declaration*);

      virtual void join_identifiers(Scope*) = 0;
      virtual void find_all_decls(Scope*) = 0;
      virtual void verify_types() = 0;

      IdPtr identifier(const std::string& token_string);

      Scope() {
        // FIXME this is dangerous, what if a Scope goes out of scope? dangling
        // pointer!
        all_scopes.push_back(this);
      }

    private:
      bool log_undeclared_identifiers() const;

      std::map<std::string, IdPtr> id_map_;
      //std::set<Declaration> decls_;
  };


  class Binop : public Expression {
    friend class KVPairList;

    public:
    // It's not clear it's okay to pass the string by reference if it's
    // referencing something in the node. Probably not worth trying because
    // this is highly unlikely to be a bottleneck.
    static NPtr build_operator(NPtrVec&& nodes, std::string op_symbol);

    static NPtr build(NPtrVec&& nodes);
    static NPtr build_paren_operator(NPtrVec&& nodes);
    static NPtr build_bracket_operator(NPtrVec&& nodes);

    virtual void join_identifiers(Scope* scope);
    virtual void verify_types();
    virtual void find_all_decls(Scope* scope);

    virtual std::string to_string(size_t n) const;
    virtual bool is_binop() const { return true; }


    virtual ~Binop(){}

    protected:
    Binop() {}
    EPtr lhs_;
    EPtr rhs_;
  };

  inline NPtr Binop::build_paren_operator(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec>(nodes), "()");
  }

  inline NPtr Binop::build_bracket_operator(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec>(nodes), "[]");
  }

  inline NPtr Binop::build(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec>(nodes), nodes[1]->token());
  }

  inline NPtr Binop::build_operator(NPtrVec&& nodes, std::string op_symbol) {
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    binop_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    binop_ptr->token_ = op_symbol;
    binop_ptr->type_ = Language::generic_operator;

    binop_ptr->precedence_ = Language::op_prec.at(op_symbol);

    return NPtr(binop_ptr);
  }


  class Terminal : public Expression {
    friend class KVPairList;

    public:
    static NPtr build(NPtrVec&& nodes, Type t);
    static NPtr build_type_literal(NPtrVec&& nodes);
    static NPtr build_string_literal(NPtrVec&& nodes);
    static NPtr build_integer(NPtrVec&& nodes);
    static NPtr build_real(NPtrVec&& nodes);

    virtual void join_identifiers(Scope*) {}
    virtual void verify_types();


    virtual std::string to_string(size_t n) const;

    protected:
    Terminal() {}
  };

  inline NPtr Terminal::build(NPtrVec&& nodes, Type t) {
    auto term_ptr = new Terminal;
    term_ptr->expr_type_ = t;
    term_ptr->token_ = nodes[0]->token();
    term_ptr->precedence_ = Language::op_prec.at("MAX");

    return NPtr(term_ptr);
  }

  inline NPtr Terminal::build_type_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Type_);
  }

  inline NPtr Terminal::build_string_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::String);
  }

  inline NPtr Terminal::build_integer(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Int);
  }

  inline NPtr Terminal::build_real(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Real);
  }



  class Assignment : public Binop {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;

      virtual void verify_types();

      virtual ~Assignment(){}

    private:
      Assignment() {}
  };

  inline NPtr Assignment::build(NPtrVec&& nodes) {
    auto assign_ptr = new Assignment;
    assign_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    assign_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    assign_ptr->token_ = ":";
    assign_ptr->type_ = Language::assign_operator;

    assign_ptr->precedence_ = Language::op_prec.at("=");

    return NPtr(assign_ptr);
  }



  class Declaration : public Binop {
    friend class Scope;

    public:
    static NPtr build(NPtrVec&& nodes);

    virtual std::string to_string(size_t n) const;
    virtual void verify_types();
    virtual void find_all_decls(Scope* scope);

    virtual ~Declaration(){}

    private:
    Declaration() {}
  };

  inline NPtr Declaration::build(NPtrVec&& nodes) {
    auto decl_ptr = new Declaration;
    decl_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    decl_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    decl_ptr->token_ = ":";
    decl_ptr->type_ = Language::decl_operator;

    decl_ptr->precedence_ = Language::op_prec.at(":");

    return NPtr(decl_ptr);
  }



  class KVPairList : public Node {
    public:
      static NPtr build_one(NPtrVec&& nodes);
      static NPtr build_more(NPtrVec&& nodes);
      static NPtr build_one_assignment_error(NPtrVec&& nodes);
      static NPtr build_more_assignment_error(NPtrVec&& nodes);


      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);

      virtual Type verify_types_with_key(Type key_type);

    private:
      KVPairList() {}

      std::vector<std::pair<EPtr, EPtr>> kv_pairs_;
  };

  inline NPtr KVPairList::build_one(NPtrVec&& nodes) {
    auto pair_list = new KVPairList;
    Expression* key_ptr;

    if (nodes[0]->node_type() == Language::reserved_else) {
      key_ptr = new Terminal;
      key_ptr->expr_type_ = Type::Bool;
      key_ptr->token_ = "else";
      key_ptr->precedence_ = Language::op_prec.at("MAX");

    } else {
      key_ptr = static_cast<Expression*>(nodes[0].release());
    }

    auto val_ptr =
      static_cast<Expression*>(nodes[2].release());

    pair_list->kv_pairs_.emplace_back(
        std::unique_ptr<Expression>(key_ptr),
        std::unique_ptr<Expression>(val_ptr));
    return NPtr(pair_list);
  }

  inline NPtr KVPairList::build_more(NPtrVec&& nodes) {
    auto pair_list = static_cast<KVPairList*>(nodes[0].release());
    Expression* key_ptr;

    if (nodes[1]->node_type() == Language::reserved_else) {
      key_ptr = new Terminal;
      key_ptr->expr_type_ = Type::Bool;
      key_ptr->token_ = "else";
      key_ptr->precedence_ = Language::op_prec.at("MAX");

    } else {
      key_ptr = static_cast<Expression*>(nodes[1].release());
    }

    auto val_ptr =
      static_cast<Expression*>(nodes[3].release());

    pair_list->kv_pairs_.emplace_back(
        std::unique_ptr<Expression>(key_ptr),
        std::unique_ptr<Expression>(val_ptr));

    return NPtr(pair_list);
  }

  inline NPtr KVPairList::build_one_assignment_error(NPtrVec&& nodes) {
    std::cerr << "You probably meant `==` instead of `=`" << std::endl;

    std::unique_ptr<Assignment> assignment_node(
        static_cast<Assignment*>(nodes[0].release()));

    // TODO this is mostly the same code as Binop::build_operator
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      std::static_pointer_cast<Expression>(assignment_node->lhs_);
    binop_ptr->rhs_ =
      std::static_pointer_cast<Expression>(assignment_node->rhs_);

    binop_ptr->token_ = "==";
    binop_ptr->type_ = Language::generic_operator;
    binop_ptr->precedence_ = Language::op_prec.at("==");

    nodes[0] = NPtr(binop_ptr);


    return build_one(std::forward<NPtrVec>(nodes));
  }

  inline NPtr KVPairList::build_more_assignment_error(NPtrVec&& nodes) {
    std::cerr << "You probably meant `==` instead of `=`" << std::endl;

    std::unique_ptr<Assignment> assignment_node(
        static_cast<Assignment*>(nodes[1].release()));

    // TODO this is mostly the same code as Binop::build_operator
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      std::static_pointer_cast<Expression>(assignment_node->lhs_);
    binop_ptr->rhs_ =
      std::static_pointer_cast<Expression>(assignment_node->rhs_);

    binop_ptr->token_ = "==";
    binop_ptr->type_ = Language::generic_operator;
    binop_ptr->precedence_ = Language::op_prec.at("==");

    nodes[0] = NPtr(binop_ptr);

    return build_more(std::forward<NPtrVec>(nodes));
  }



  class Case : public Expression {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void find_all_decls(Scope*) { // TODO
      }
      virtual void verify_types();

    private:
      Case() {}
      std::unique_ptr<KVPairList> pairs_;
  };

  inline NPtr Case::build(NPtrVec&& nodes) {
    auto output = new Case;
    output->pairs_ =
      std::unique_ptr<KVPairList>(static_cast<KVPairList*>(nodes[3].release()));
    return NPtr(output);
  }



  class Identifier : public Terminal {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual bool is_identifier() const { return true; }

      virtual std::string to_string(size_t n) const;

      Identifier(const std::string& token_string) {
        token_ = token_string;
        expr_type_ = Type::Unknown;
        precedence_ = Language::op_prec.at("MAX");
      }
  };

  inline NPtr Identifier::build(NPtrVec&& nodes) {
    return NPtr(new Identifier(nodes[0]->token()));
  }

  inline std::string Identifier::to_string(size_t n) const {
    std::string spaces;
    for (size_t i = 0; i < n; ++i) {
      spaces += "  ";
    }
 
    return spaces + "<Identifier (" + expr_type_.to_string() + "): " + token() + ">\n";
  }


  class Statements : public Node {
    friend class AnonymousScope;

    public:
      static NPtr build_one(NPtrVec&& nodes);
      static NPtr build_more(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void verify_types();
      virtual void find_all_decls(Scope* scope);

      inline size_t size() { return statements_.size(); }

    private:
      Statements() {}
      std::vector<EPtr> statements_;
  };

  inline NPtr Statements::build_one(NPtrVec&& nodes) {
    auto output = new Statements;
    output->statements_.emplace_back(static_cast<Expression*>(nodes[0].release()));

    return NPtr(output);
  }

  inline NPtr Statements::build_more(NPtrVec&& nodes) {
    auto output = static_cast<Statements*>(nodes[0].release());
    output->statements_.emplace_back(static_cast<Expression*>(nodes[1].release()));

    return NPtr(output);
  }




  class AnonymousScope : public Expression, public Scope {
    public:
      static NPtr build(NPtrVec&& nodes);
      static std::unique_ptr<AnonymousScope> build_empty();


      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void verify_types();
      virtual void find_all_decls(Scope* scope);

      void add_statements(NPtr&& stmts_ptr);

    protected:
      AnonymousScope() {}
      std::unique_ptr<Statements> statements_;
  };

  inline std::unique_ptr<AnonymousScope> AnonymousScope::build_empty() {
    std::unique_ptr<AnonymousScope> anon_scope(new AnonymousScope);
    anon_scope->statements_ = std::unique_ptr<Statements>(new Statements);

    return anon_scope;
  }

  inline NPtr AnonymousScope::build(NPtrVec&& nodes) {
    auto anon_scope = new AnonymousScope;

    anon_scope->statements_ =
      std::unique_ptr<Statements>(static_cast<Statements*>(nodes[1].release()));

    return NPtr(anon_scope);
  }


}  // namespace AST

#endif  // ICARUS_AST_NODE_H
