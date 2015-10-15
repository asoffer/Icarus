#ifndef ICARUS_AST_H
#define ICARUS_AST_H

#include <string>
#include <iostream>
#include <map>
#include <set>
#include <vector>

// TODO Figure out what you need from this.
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "Language.h"
#include "Type.h"
#include "typedefs.h"

namespace AST {
  extern llvm::IRBuilder<> builder;


  class Scope;
  class Declaration;
  //class AnonymousScope;

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
      virtual llvm::Value* generate_code(Scope*) { return nullptr; }

      virtual bool is_identifier() const {
        return type_ == Language::identifier;
      }

      bool is_return() const {
        return node_type() == Language::return_expression;
      }
      virtual bool is_binop() const { return false; }
      virtual bool is_chain_op() const { return false; }
      virtual bool is_declaration() const { return false; }

      Node(Language::NodeType type = Language::unknown,
          const std::string& token = "") : type_(type), token_(token) {}

      virtual ~Node(){}


      inline friend std::ostream& operator<<(
          std::ostream& os, const Node& node) {

        return os << node.to_string(0);
      }

    protected:
      Language::NodeType type_;
      std::string token_;
  };



  class Scope {
    friend class FunctionLiteral;

    public:
    static std::vector<Scope*> scope_registry;

    Scope();
    static Scope* make_global();

    void determine_declared_types();
    void verify_no_shadowing() const;


    void register_input(Declaration*);
    void register_local(Declaration*);
    void attach_to_parent(Scope* parent_scope);

    IdPtr get_identifier(const std::string& token_string);

    void generate_stack_variables(llvm::Function* fn);


    private:
    // TODO should this be here? Maybe move it to FunctionLiteral?
    llvm::BasicBlock* block_;

    // A lookup for all of the inputs to this scope. If this is not a
    // function, inputs_ will be empty.
    std::map<std::string, IdPtr> inputs_;

    // A lookup for all the local variables. Every local variable will also be
    // declared in this scope and therefore have an entry in decl_registry_ as
    // well.
    std::map<std::string, IdPtr> locals_;

    // A registry of all declarations made in this scope and not in any deeper
    // scope
    std::map<std::string, EPtr> decl_registry_;

    Scope* parent_;
    std::vector<Scope*> children_;
  };



  class Expression : public Node {
    friend class KVPairList;
    friend class Unop;
    friend class Binop;
    friend class ChainOp;
    friend class Declaration;
    friend class Assignment;
    friend class Scope;

    public:
    static std::unique_ptr<Node> parenthesize(NPtrVec&& nodes);

    size_t precedence() const { return precedence_; }

    virtual void join_identifiers(Scope* scope) = 0;
    virtual void verify_types() = 0;
    virtual Type interpret_as_type() const = 0;
    virtual Type type() const { return expr_type_; }

    virtual void verify_type_is(Type t);

    virtual llvm::Value* generate_code(Scope*) = 0;

    virtual void find_all_decls(Scope*) {}

    virtual ~Expression(){}

    protected:
    Expression() {}

    size_t precedence_;
    Type expr_type_;
  };

  inline std::unique_ptr<Node> Expression::parenthesize(NPtrVec&& nodes) {
    auto expr_ptr = static_cast<Expression*>(nodes[1].release());
    expr_ptr->precedence_ = Language::op_prec.at("MAX");
    return std::unique_ptr<Node>(expr_ptr);
  }


  inline void Expression::verify_type_is(Type t) {
    verify_types();

    if (expr_type_ != t) {
      // TODO: give some context for this error message. Why must this be the
      // type?  So far the only instance where this is called is for case
      // statements,
      std::cerr
        << "Type of `____` must be " << t.to_string() << ", but "
        << expr_type_.to_string() << " found instead." << std::endl;
      expr_type_ = t;
    }
  }

  // TODO: This only represents a left unary operator for now
  class Unop : public Expression {
    friend class Statements;

    public:
    static std::unique_ptr<Node> build(NPtrVec&& nodes);

    virtual void join_identifiers(Scope* scope);
    virtual void verify_types();
    virtual void find_all_decls(Scope* scope);
    virtual Type interpret_as_type() const;

    virtual std::string to_string(size_t n) const;

    virtual llvm::Value* generate_code(Scope*);

    private:
    EPtr expr_;
  };


  inline std::unique_ptr<Node> Unop::build(NPtrVec&& nodes) {
    auto unop_ptr = new Unop;
    unop_ptr->expr_ =
      EPtr(static_cast<Expression*>(nodes[1].release()));

    unop_ptr->type_ = Language::expression;
    if (nodes[0]->node_type() == Language::reserved_return) {
      unop_ptr->token_ = "return";
    } else {
      unop_ptr->token_ = nodes[0]->token();
    }

    unop_ptr->precedence_ = Language::op_prec.at(unop_ptr->token());

    return std::unique_ptr<Node>(unop_ptr);

  }



  class Binop : public Expression {
    friend class KVPairList;
    friend class FunctionLiteral;

    public:
    static std::unique_ptr<Node> build_operator(NPtrVec&& nodes, std::string op_symbol);

    static std::unique_ptr<Node> build(NPtrVec&& nodes);
    static std::unique_ptr<Node> build_paren_operator(NPtrVec&& nodes);
    static std::unique_ptr<Node> build_bracket_operator(NPtrVec&& nodes);

    virtual void join_identifiers(Scope* scope);
    virtual void verify_types();
    virtual void find_all_decls(Scope* scope);
    virtual Type interpret_as_type() const;

    virtual llvm::Value* generate_code(Scope*);

    virtual std::string to_string(size_t n) const;
    virtual bool is_binop() const { return true; }


    virtual ~Binop(){}

    protected:
    Binop() {}
    EPtr lhs_;
    EPtr rhs_;
  };

  inline std::unique_ptr<Node> Binop::build_paren_operator(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec>(nodes), "()");
  }

  inline std::unique_ptr<Node> Binop::build_bracket_operator(NPtrVec&& nodes) {
    return Binop::build_operator(std::forward<NPtrVec>(nodes), "[]");
  }

  inline std::unique_ptr<Node> Binop::build(NPtrVec&& nodes) {
    return Binop::build_operator(
        std::forward<NPtrVec>(nodes),
        nodes[1]->token());
  }

  inline std::unique_ptr<Node> Binop::build_operator(NPtrVec&& nodes, std::string op_symbol) {
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    binop_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    binop_ptr->token_ = op_symbol;
    binop_ptr->type_ = Language::generic_operator;

    binop_ptr->precedence_ = Language::op_prec.at(op_symbol);

    return std::unique_ptr<Node>(binop_ptr);
  }



  class ChainOp : public Expression {
    public:
      static std::unique_ptr<Node> build(NPtrVec&& nodes);

      virtual void join_identifiers(Scope* scope);
      virtual void verify_types();
      virtual void find_all_decls(Scope* scope);
      virtual Type interpret_as_type() const;

      virtual llvm::Value* generate_code(Scope*);

      virtual std::string to_string(size_t n) const;
      virtual bool is_chain_op() const { return true; }

    private:
      std::vector<NPtr> ops_;
      std::vector<EPtr> exprs_;
  };

  inline std::unique_ptr<Node> ChainOp::build(NPtrVec&& nodes) {
    std::unique_ptr<ChainOp> chain_ptr(nullptr);

    // Add to a chain so long as the precedence levels match. The only thing at
    // that precedence level should be the operators which can be chained.
    bool use_old_chain_op = nodes[0]->is_chain_op();
    if (use_old_chain_op) {
     ChainOp* lhs_ptr = static_cast<ChainOp*>(nodes[0].get());

     if (lhs_ptr->precedence() != Language::op_prec.at(nodes[1]->token())) {
       use_old_chain_op = false;
     }
    }
    if (use_old_chain_op) {
      chain_ptr = std::unique_ptr<ChainOp>(
          static_cast<ChainOp*>(nodes[0].release()));

    } else {
      chain_ptr = std::unique_ptr<ChainOp>(new ChainOp);
      chain_ptr->exprs_.emplace_back(
          static_cast<Expression*>(nodes[0].release()));
      chain_ptr->precedence_ = Language::op_prec.at(nodes[1]->token());
    }

    chain_ptr->ops_.push_back(std::move(nodes[1]));

    chain_ptr->exprs_.emplace_back(
        static_cast<Expression*>(nodes[2].release()));

    return std::unique_ptr<Node>(chain_ptr.release());
  }




  class Terminal : public Expression {
    friend class KVPairList;

    public:
    static std::unique_ptr<Node> build(NPtrVec&& nodes, Type t);
    static std::unique_ptr<Node> build_type_literal(NPtrVec&& nodes);
    static std::unique_ptr<Node> build_string_literal(NPtrVec&& nodes);
    static std::unique_ptr<Node> build_integer_literal(NPtrVec&& nodes);
    static std::unique_ptr<Node> build_real_literal(NPtrVec&& nodes);
    static std::unique_ptr<Node> build_character_literal(NPtrVec&& nodes);

    virtual void join_identifiers(Scope*) {}
    virtual void verify_types();
    virtual Type interpret_as_type() const;

    virtual llvm::Value* generate_code(Scope*);

    virtual std::string to_string(size_t n) const;

    protected:
    Terminal() {}
  };

  inline std::unique_ptr<Node> Terminal::build(NPtrVec&& nodes, Type t) {
    auto term_ptr = new Terminal;
    term_ptr->expr_type_ = t;
    term_ptr->token_ = nodes[0]->token();
    term_ptr->precedence_ = Language::op_prec.at("MAX");

    return std::unique_ptr<Node>(term_ptr);
  }

  inline std::unique_ptr<Node> Terminal::build_type_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Type_);
  }

  inline std::unique_ptr<Node> Terminal::build_string_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::String);
  }

  inline std::unique_ptr<Node> Terminal::build_integer_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Int);
  }

  inline std::unique_ptr<Node> Terminal::build_real_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Real);
  }

  inline std::unique_ptr<Node> Terminal::build_character_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::Char);
  }


  class Assignment : public Binop {
    public:
      static std::unique_ptr<Node> build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;

      virtual void verify_types();

      virtual llvm::Value* generate_code(Scope*);

      virtual ~Assignment(){}

    private:
      Assignment() {}
  };

  inline std::unique_ptr<Node> Assignment::build(NPtrVec&& nodes) {
    auto assign_ptr = new Assignment;
    assign_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    assign_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    assign_ptr->token_ = ":";
    assign_ptr->type_ = Language::assign_operator;

    assign_ptr->precedence_ = Language::op_prec.at("=");

    return std::unique_ptr<Node>(assign_ptr);
  }



  class Declaration : public Binop {
    friend class Scope;

    public:
    static std::unique_ptr<Node> build(NPtrVec&& nodes);

    std::string identifier() const { return lhs_->token(); }
    EPtr declared_type() const { return rhs_; }

    virtual std::string to_string(size_t n) const;
    virtual void verify_types();
    virtual void find_all_decls(Scope* scope);

    virtual llvm::Value* generate_code(Scope*);

    virtual bool is_declaration() const { return true; }

    virtual ~Declaration(){}

    private:
    Declaration() {}
  };

  inline std::unique_ptr<Node> Declaration::build(NPtrVec&& nodes) {
    auto decl_ptr = new Declaration;
    decl_ptr->lhs_ =
      EPtr(static_cast<Expression*>(nodes[0].release()));

    decl_ptr->rhs_ =
      EPtr(static_cast<Expression*>(nodes[2].release()));

    decl_ptr->token_ = ":";
    decl_ptr->type_ = Language::decl_operator;

    decl_ptr->precedence_ = Language::op_prec.at(":");

    return std::unique_ptr<Node>(decl_ptr);
  }



  class KVPairList : public Node {
    public:
      static std::unique_ptr<Node> build_one(NPtrVec&& nodes);
      static std::unique_ptr<Node> build_more(NPtrVec&& nodes);
      static std::unique_ptr<Node> build_one_assignment_error(NPtrVec&& nodes);
      static std::unique_ptr<Node> build_more_assignment_error(NPtrVec&& nodes);


      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);

      virtual Type verify_types_with_key(Type key_type);

    private:
      KVPairList() {}

      std::vector<std::pair<EPtr, EPtr>> kv_pairs_;
  };

  inline std::unique_ptr<Node> KVPairList::build_one(NPtrVec&& nodes) {
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
    return std::unique_ptr<Node>(pair_list);
  }

  inline std::unique_ptr<Node> KVPairList::build_more(NPtrVec&& nodes) {
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

    return std::unique_ptr<Node>(pair_list);
  }

  inline std::unique_ptr<Node> KVPairList::build_one_assignment_error(NPtrVec&& nodes) {
    std::cerr << "You probably meant `==` instead of `=`" << std::endl;

    std::unique_ptr<Assignment> assignment_node(
        static_cast<Assignment*>(nodes[0].release()));

    // TODO this is mostly the same code as Binop::build_operator
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ = std::move(assignment_node->lhs_);
    binop_ptr->rhs_ = std::move(assignment_node->rhs_);

    binop_ptr->token_ = "==";
    binop_ptr->type_ = Language::generic_operator;
    binop_ptr->precedence_ = Language::op_prec.at("==");

    nodes[0] = std::unique_ptr<Node>(binop_ptr);


    return build_one(std::forward<NPtrVec>(nodes));
  }

  inline std::unique_ptr<Node> KVPairList::build_more_assignment_error(NPtrVec&& nodes) {
    std::cerr << "You probably meant `==` instead of `=`" << std::endl;

    std::unique_ptr<Assignment> assignment_node(
        static_cast<Assignment*>(nodes[1].release()));

    // TODO this is mostly the same code as Binop::build_operator
    auto binop_ptr = new Binop;
    binop_ptr->lhs_ = std::move(assignment_node->lhs_);
    binop_ptr->rhs_ = std::move(assignment_node->rhs_);

    binop_ptr->token_ = "==";
    binop_ptr->type_ = Language::generic_operator;
    binop_ptr->precedence_ = Language::op_prec.at("==");

    nodes[0] = std::unique_ptr<Node>(binop_ptr);

    return build_more(std::forward<NPtrVec>(nodes));
  }



  class Case : public Expression {
    public:
      static std::unique_ptr<Node> build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void find_all_decls(Scope*);
      virtual void verify_types();
      virtual Type interpret_as_type() const;

      virtual llvm::Value* generate_code(Scope*);

    private:
      Case() {}
      std::unique_ptr<KVPairList> pairs_;
  };

  inline std::unique_ptr<Node> Case::build(NPtrVec&& nodes) {
    auto output = new Case;
    output->pairs_ =
      std::unique_ptr<KVPairList>(
          static_cast<KVPairList*>(nodes[3].release()));
    return std::unique_ptr<Node>(output);
  }


  class Identifier : public Terminal {
    friend class Scope;
    friend class Assignment;

    public:
      static std::unique_ptr<Node> build(NPtrVec&& nodes) {
        return std::unique_ptr<Node>(new Identifier(nodes[0]->token()));
      }

      virtual bool is_identifier() const { return true; }
      virtual std::string to_string(size_t n) const;

      virtual void verify_types();
      virtual llvm::Value* generate_code(Scope*);

      Identifier(const std::string& token_string) : val_(nullptr) {
        token_ = token_string;
        expr_type_ = Type::Unknown;
        precedence_ = Language::op_prec.at("MAX");
      }

    private:
      llvm::AllocaInst* val_;
  };



  class Statements : public Node {
    //friend class AnonymousScope;

    public:
    static std::unique_ptr<Node> build_one(NPtrVec&& nodes);
    static std::unique_ptr<Node> build_more(NPtrVec&& nodes);

    virtual std::string to_string(size_t n) const;
    virtual void join_identifiers(Scope* scope);
    virtual void verify_types();
    virtual void find_all_decls(Scope* scope);
    void collect_return_types(std::set<Type>* return_exprs) const;
    llvm::Value* generate_code(Scope* scope);

    inline size_t size() { return statements_.size(); }

    private:
    Statements() {}
    std::vector<NPtr> statements_;
  };

  inline std::unique_ptr<Node> Statements::build_one(NPtrVec&& nodes) {
    auto output = new Statements;
    output->statements_.emplace_back(nodes[0].release());

    return std::unique_ptr<Node>(output);
  }

  inline std::unique_ptr<Node> Statements::build_more(NPtrVec&& nodes) {
    auto output = static_cast<Statements*>(nodes[0].release());
    output->statements_.emplace_back(nodes[1].release());

    return std::unique_ptr<Node>(output);
  }


/*
  class AnonymousScope : public Expression, public Scope {
    friend class FunctionLiteral;
    friend class Unop;

    public:
    static std::unique_ptr<Node> build(NPtrVec&& nodes);
    static std::unique_ptr<AnonymousScope> build_empty();

    virtual std::string to_string(size_t n) const;
    virtual void join_identifiers(Scope* scope);
    virtual void verify_types();
    virtual void find_all_decls(Scope* scope);
    virtual Type interpret_as_type() const;

    virtual llvm::Value* generate_code(Scope*);

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

  inline std::unique_ptr<Node> AnonymousScope::build(NPtrVec&& nodes) {
    auto anon_scope = new AnonymousScope;

    anon_scope->statements_ = std::unique_ptr<Statements>(
        static_cast<Statements*>(nodes[1].release()));

    return std::unique_ptr<Node>(anon_scope);
  }
*/


  class FunctionLiteral : public Expression {
    public:
      static std::unique_ptr<Node> build(NPtrVec&& nodes) {
        auto fn_lit = new FunctionLiteral;

        auto stmts_ptr = static_cast<Statements*>(nodes[2].release());
        fn_lit->statements_ = std::unique_ptr<Statements>(stmts_ptr);

        // TODO scopes inside these statements should point to fn_scope_.

        auto binop_ptr = static_cast<Binop*>(nodes[0].release());

        fn_lit->return_type_ = std::move(binop_ptr->rhs_);

        // TODO What if the fn_expression is more complicated, like a function
        // of the form (int -> int) -> int? I'm not sure how robust this is
        if (!binop_ptr->lhs_->is_declaration()) {
          // TODO Is this error message correct?
          std::cerr
            << "No input parameters named in function declaration"
            << std::endl;
        } else {
          // TODO This assumes a single declaration, not a comma-separated list
          auto decl_ptr = std::static_pointer_cast<Declaration>(binop_ptr->lhs_);

          fn_lit->fn_scope_.inputs_[decl_ptr->identifier()] =
            IdPtr(new Identifier(decl_ptr->identifier()));

          fn_lit->fn_scope_.decl_registry_[decl_ptr->identifier()] = decl_ptr->declared_type();
        }

        delete binop_ptr;

        return std::unique_ptr<Node>(fn_lit);
      }

      virtual void find_all_decls(Scope*);
      virtual void join_identifiers(Scope* scope);
      virtual void verify_types();
      virtual Type interpret_as_type() const;

      virtual llvm::Value* generate_code(Scope*);

      virtual std::string to_string(size_t n) const;

    private:
      Scope fn_scope_;
      EPtr return_type_;
      std::unique_ptr<Statements> statements_;

      FunctionLiteral() {}
      virtual ~FunctionLiteral() {}
  };

  
  class While : public Node {
    public:
      static std::unique_ptr<Node> build(NPtrVec&& nodes) {
        auto while_stmt = new While;
        while_stmt->cond_ = EPtr(static_cast<Expression*>(nodes[1].release()));
        while_stmt->statements_ = std::unique_ptr<Statements>(
            static_cast<Statements*>(nodes[3].release()));
        return std::unique_ptr<Node>(while_stmt);
      }

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
 
    private:
      EPtr cond_;
      std::unique_ptr<Statements> statements_;
      Scope body_scope_;
  };


}  // namespace AST

#endif  // ICARUS_AST_NODE_H
