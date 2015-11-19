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
#include "ScopeDB.h"
#include "ErrorLog.h"

extern ErrorLog error_log;

namespace AST {
  using ::ScopeDB::Scope;

  class Node {
    public:
      friend class KVPairList;
      friend class Expression;
      friend class Terminal;
      friend class Identifier;
      friend class Unop;
      friend class FunctionLiteral;
      friend class Case;
      friend class Binop;
      friend class ChainOp;
      friend class Declaration;
      friend class Statements;
      friend class Assignment;
      friend class ArrayType;

      static inline Node eof_node(size_t line_num) { return Node(line_num, Language::eof, ""); }
      static inline Node newline_node() { return Node(0, Language::newline, ""); }
      static inline Node string_literal_node(size_t line_num, const std::string& str_lit) { 
        return Node(line_num, Language::string_literal, str_lit);
      }

      Language::NodeType node_type() const { return type_; }
      void set_node_type(Language::NodeType t) { type_ = t; }

      std::string token() const { return token_; }
      void set_token(const std::string& token_string) {
        token_ = token_string;
      }
      size_t line_num() const { return line_num_; }

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope) {}
      virtual void assign_decl_to_scope(Scope* scope) {}
      virtual void record_dependencies(EPtr eptr) const {}
      virtual void verify_types() {}

      virtual llvm::Value* generate_code(Scope* scope) { return nullptr; }

      virtual bool is_identifier() const {
        return type_ == Language::identifier;
      }
      virtual bool is_expression() const { return false; }

      bool is_return() const {
        return node_type() == Language::return_expression;
      }
      bool is_print() const {
        return node_type() == Language::print_expression;
      }
 
      virtual bool is_binop() const { return false; }
      virtual bool is_chain_op() const { return false; }
      virtual bool is_comma_list() const { return false; }
      virtual bool is_declaration() const { return false; }

      Node(size_t line_num = 0, Language::NodeType type = Language::unknown, const std::string& token = "")
        : type_(type), token_(token), line_num_(line_num) {}

      virtual ~Node(){}


      inline friend std::ostream& operator<<(std::ostream& os, const Node& node) {
        return os << node.to_string(0);
      }

    protected:
      Language::NodeType type_;
      std::string token_;
      size_t line_num_;
  };


  class Expression : public Node {
    friend void ScopeDB::Scope::determine_declared_types();
    friend class KVPairList;
    friend class Unop;
    friend class Binop;
    friend class ChainOp;
    friend class Declaration;
    friend class Assignment;
    friend class ArrayType;

    public:
    static NPtr parenthesize(NPtrVec&& nodes);

    size_t precedence() const { return precedence_; }

    virtual std::string to_string(size_t n) const = 0;
    virtual void join_identifiers(Scope* scope) = 0;
    virtual void assign_decl_to_scope(Scope* scope) = 0;
    virtual void record_dependencies(EPtr eptr) const = 0;
    virtual void verify_types() = 0;

    virtual Type* interpret_as_type() const = 0;
    virtual llvm::Value* generate_code(Scope* scope) = 0;
    virtual llvm::Value* generate_lvalue(Scope* scope) = 0;

    virtual Type* type() const { return expr_type_; }

    virtual bool is_expression() const { return true; }


    virtual ~Expression(){}

    protected:
    Expression() : expr_type_(Type::get_unknown()) {}

    size_t precedence_;
    Type* expr_type_;
  };

  inline NPtr Expression::parenthesize(NPtrVec&& nodes) {
    auto expr_ptr = std::static_pointer_cast<Expression>(nodes[1]);
    expr_ptr->precedence_ = Language::op_prec.at("MAX");
    return expr_ptr;
  }

  // TODO: This only represents a left unary operator for now
  class Unop : public Expression {
    friend class Statements;

    public:
    static NPtr build(NPtrVec&& nodes);
    static NPtr build_paren_operator(NPtrVec&& nodes);

    virtual std::string to_string(size_t n) const;
    virtual void join_identifiers(Scope* scope);
    virtual void assign_decl_to_scope(Scope* scope);
    virtual void record_dependencies(EPtr eptr) const;
    virtual void verify_types();

    virtual Type* interpret_as_type() const;


    virtual llvm::Value* generate_code(Scope* scope);
    virtual llvm::Value* generate_lvalue(Scope* scope);

    private:
    EPtr expr_;
  };


  inline NPtr Unop::build(NPtrVec&& nodes) {
    auto unop_ptr = new Unop;
    unop_ptr->expr_ = std::static_pointer_cast<Expression>(nodes[1]);
    unop_ptr->line_num_ = nodes[0]->line_num_;

    unop_ptr->type_ = Language::expression;
    if (nodes[0]->node_type() == Language::reserved_return) {
      unop_ptr->token_ = "return";
      
    }
    else if (nodes[0]->node_type() == Language::reserved_return) {
      unop_ptr->token_ = "print";

    } else {
      unop_ptr->token_ = nodes[0]->token();
    }

    unop_ptr->precedence_ = Language::op_prec.at(unop_ptr->token());

    return NPtr(unop_ptr);
  }

  inline NPtr Unop::build_paren_operator(NPtrVec&& nodes) {
    auto unop_ptr = new Unop;
    unop_ptr->line_num_ = nodes[1]->line_num_;

    unop_ptr->expr_ =
      std::static_pointer_cast<Expression>(nodes[0]);

    unop_ptr->token_ = "()";
    unop_ptr->type_ = Language::expression;

    unop_ptr->precedence_ = Language::op_prec.at("()");

    return NPtr(unop_ptr);
  }


  class Binop : public Expression {
    friend class KVPairList;
    friend class FunctionLiteral;

    public:
    static NPtr build_operator(NPtrVec&& nodes, std::string op_symbol);

    static NPtr build(NPtrVec&& nodes);
    static NPtr build_paren_operator(NPtrVec&& nodes);
    static NPtr build_bracket_operator(NPtrVec&& nodes);
    static NPtr build_array_type(NPtrVec&& nodes);

    virtual std::string to_string(size_t n) const;
    virtual void join_identifiers(Scope* scope);
    virtual void assign_decl_to_scope(Scope* scope);
    virtual void record_dependencies(EPtr eptr) const;
    virtual void verify_types();

    virtual Type* interpret_as_type() const;
    virtual llvm::Value* generate_code(Scope* scope);
    virtual llvm::Value* generate_lvalue(Scope* scope);

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
    return Binop::build_operator(
        std::forward<NPtrVec>(nodes),
        nodes[1]->token());
  }

  inline NPtr Binop::build_operator(NPtrVec&& nodes, std::string op_symbol) {
    auto binop_ptr = new Binop;
    binop_ptr->line_num_ = nodes[1]->line_num_;

    binop_ptr->lhs_ =
      std::static_pointer_cast<Expression>(nodes[0]);

    binop_ptr->rhs_ =
      std::static_pointer_cast<Expression>(nodes[2]);

    binop_ptr->token_ = op_symbol;
    binop_ptr->type_ = Language::generic_operator;

    binop_ptr->precedence_ = Language::op_prec.at(op_symbol);

    return NPtr(binop_ptr);
  }


  class ChainOp : public Expression {
    public:
      friend class ArrayType;

      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual Type* interpret_as_type() const;

      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);

      virtual bool is_chain_op() const { return true; }
      virtual bool is_comma_list() const { return ops_.front()->token() == ","; }

    private:
      std::vector<NPtr> ops_;
      std::vector<EPtr> exprs_;
  };

  inline NPtr ChainOp::build(NPtrVec&& nodes) {
    std::shared_ptr<ChainOp> chain_ptr(nullptr);

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
      chain_ptr = std::static_pointer_cast<ChainOp>(nodes[0]);

    } else {
      chain_ptr = std::shared_ptr<ChainOp>(new ChainOp);
      chain_ptr->line_num_ = nodes[1]->line_num_;

      chain_ptr->exprs_.push_back(std::static_pointer_cast<Expression>(nodes[0]));
      chain_ptr->precedence_ = Language::op_prec.at(nodes[1]->token());
    }

    chain_ptr->ops_.push_back(nodes[1]);

    chain_ptr->exprs_.push_back(
        std::static_pointer_cast<Expression>(nodes[2]));

    return std::static_pointer_cast<Node>(chain_ptr);
  }


  class ArrayType : public Expression {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual Type* interpret_as_type() const;
      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);



    private:
      EPtr len_;
      EPtr array_type_;
  };

  inline NPtr ArrayType::build(NPtrVec&& nodes) {
    if (nodes[1]->is_comma_list()) {
      auto len_chain = std::static_pointer_cast<ChainOp>(nodes[1]);

      auto iter = len_chain->exprs_.rbegin();
      EPtr prev = std::static_pointer_cast<Expression>(nodes[3]);
      while (iter != len_chain->exprs_.rend()) {
        auto array_type_ptr = new ArrayType;
        array_type_ptr->line_num_ = (*iter)->line_num();
        array_type_ptr->len_ = *iter;

        array_type_ptr->token_ = ""; // TODO what should go here? Does it matter?
        array_type_ptr->precedence_ = Language::op_prec.at("MAX");

        array_type_ptr->array_type_ = prev;
        prev = EPtr(array_type_ptr);
        ++iter;
      }
      return std::static_pointer_cast<Node>(prev);

    } else {
      auto array_type_ptr = new ArrayType;
      array_type_ptr->line_num_ = nodes[0]->line_num_;

      array_type_ptr->len_ =
        std::static_pointer_cast<Expression>(nodes[1]);

      array_type_ptr->array_type_ =
        std::static_pointer_cast<Expression>(nodes[3]);

      array_type_ptr->token_ = ""; // TODO what should go here? Does it matter?
      array_type_ptr->precedence_ = Language::op_prec.at("MAX");

      return NPtr(array_type_ptr);
    }
  }


  class Terminal : public Expression {
    public:
      friend class KVPairList;

      static NPtr build(NPtrVec&& nodes, Type* t);
      static NPtr build_type_literal(NPtrVec&& nodes);
      static NPtr build_string_literal(NPtrVec&& nodes);
      static NPtr build_bool_literal(NPtrVec&& nodes);
      static NPtr build_integer_literal(NPtrVec&& nodes);
      static NPtr build_real_literal(NPtrVec&& nodes);
      static NPtr build_character_literal(NPtrVec&& nodes);
      static NPtr build_void_return(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope) {}
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual Type* interpret_as_type() const;
      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);

    protected:
      Terminal() {}
  };

  inline NPtr Terminal::build(NPtrVec&& nodes, Type* t) {
    auto term_ptr = new Terminal;
    term_ptr->line_num_ = nodes[0]->line_num_;
    term_ptr->expr_type_ = t;
    term_ptr->token_ = nodes[0]->token();
    term_ptr->precedence_ = Language::op_prec.at("MAX");

    return NPtr(term_ptr);
  }

  inline NPtr Terminal::build_type_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::get_type());
  }

  inline NPtr Terminal::build_string_literal(NPtrVec&& nodes) {
    // FIXME implement strings
    return build(std::forward<NPtrVec>(nodes), Type::get_type_error());
  }

  inline NPtr Terminal::build_bool_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::get_bool());
  }

  inline NPtr Terminal::build_integer_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::get_int());
  }

  inline NPtr Terminal::build_real_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::get_real());
  }

  inline NPtr Terminal::build_character_literal(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes), Type::get_char());
  }

  inline NPtr Terminal::build_void_return(NPtrVec&& nodes) {
    // FIXME implement strings
    return build(std::forward<NPtrVec>(nodes), Type::get_void());
  }




  class Assignment : public Binop {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void verify_types();

      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);

      virtual ~Assignment(){}

    private:
      Assignment() {}
  };

  inline NPtr Assignment::build(NPtrVec&& nodes) {
    auto assign_ptr = new Assignment;
    assign_ptr->line_num_ = nodes[1]->line_num_;

    assign_ptr->lhs_ = std::static_pointer_cast<Expression>(nodes[0]);
    assign_ptr->rhs_ = std::static_pointer_cast<Expression>(nodes[2]);

    assign_ptr->token_ = nodes[1]->token();
    assign_ptr->type_ = Language::assign_operator;

    assign_ptr->precedence_ = Language::op_prec.at(assign_ptr->token_);

    return NPtr(assign_ptr);
  }


  class Identifier : public Terminal {
    friend class Assignment;

    public:
      static NPtr build(NPtrVec&& nodes) {
        return NPtr(new Identifier(nodes[0]->line_num_, nodes[0]->token()));
      }

      virtual std::string to_string(size_t n) const;
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual bool is_identifier() const { return true; }
      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);

      Identifier(size_t line_num, const std::string& token_string) : alloca_(nullptr) {
        token_ = token_string;
        expr_type_ = Type::get_unknown();
        precedence_ = Language::op_prec.at("MAX");
        line_num_ = line_num;
      }

      llvm::AllocaInst* alloca_;
  };


  // class Declaration
  //
  // Represents declarations that may or may not be made with type inference.
  // Either of these can be represented.
  //
  // identifier : type
  // identifer := value
  class Declaration : public Expression {
    public:
      friend DeclPtr ScopeDB::make_declaration(size_t line_num, const std::string& id_string);
      friend class Scope;

      static NPtr build(NPtrVec&& nodes,
          const std::string&, Language::NodeType node_type, bool infer);
      static NPtr build_decl(NPtrVec&& nodes);
      static NPtr build_assign(NPtrVec&& nodes);

      std::string identifier_string() const { return id_->token(); }
      IdPtr declared_identifier() const { return id_; }
      EPtr declared_type() const { return decl_type_; }

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      bool type_is_inferred() const { return infer_type_; }
      virtual Type* interpret_as_type() const {
        return decl_type_->interpret_as_type();
      }

      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);

      virtual bool is_declaration() const { return true; }

      virtual ~Declaration(){}


      Declaration() {}

    private:
      // The identifier being declared
      IdPtr id_;

      // May represent the declared type or the value whose type is being
      // inferred
      EPtr decl_type_;
      Scope* scope_;
      bool infer_type_;
  };

  inline NPtr Declaration::build(NPtrVec&& nodes,
      const std::string& op, Language::NodeType node_type, bool infer) {

    auto decl_ptr = ScopeDB::make_declaration(nodes[1]->line_num_, nodes[0]->token());
    decl_ptr->decl_type_ = std::static_pointer_cast<Expression>(nodes[2]);

    decl_ptr->token_ = op;
    decl_ptr->type_ = node_type;

    decl_ptr->precedence_ = Language::op_prec.at(op);
    decl_ptr->infer_type_ = infer;

    return std::static_pointer_cast<Node>(decl_ptr);
  }

  inline NPtr Declaration::build_decl(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes),
        ":", Language::decl_operator, false);
  }

  inline NPtr Declaration::build_assign(NPtrVec&& nodes) {
    return build(std::forward<NPtrVec>(nodes),
        ":=", Language::decl_assign_operator, true);
  }

  class KVPairList : public Node {
    public:
      friend class Case;
      static NPtr build_one(NPtrVec&& nodes);
      static NPtr build_more(NPtrVec&& nodes);
      static NPtr build_one_assignment_error(NPtrVec&& nodes);
      static NPtr build_more_assignment_error(NPtrVec&& nodes);


      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual Type* verify_types_with_key(Type* key_type);

      inline size_t size() const { return kv_pairs_.size(); }

    private:
      KVPairList() {}

      std::vector<std::pair<EPtr, EPtr>> kv_pairs_;
  };

  inline NPtr KVPairList::build_one(NPtrVec&& nodes) {
    auto pair_list = new KVPairList;
    pair_list->line_num_ = nodes[0]->line_num_;
    EPtr key_ptr;

    if (nodes[0]->node_type() == Language::reserved_else) {
      key_ptr = EPtr(new Terminal);
      // TODO line num
      key_ptr->expr_type_ = Type::get_bool();
      key_ptr->token_ = "else";
      key_ptr->precedence_ = Language::op_prec.at("MAX");

    } else {
      key_ptr = std::static_pointer_cast<Expression>(nodes[0]);
    }

    auto val_ptr = std::static_pointer_cast<Expression>(nodes[2]);

    pair_list->kv_pairs_.emplace_back(key_ptr, val_ptr);
    return NPtr(pair_list);
  }

  inline NPtr KVPairList::build_more(NPtrVec&& nodes) {
    auto pair_list = std::static_pointer_cast<KVPairList>(nodes[0]);
    EPtr key_ptr;

    if (nodes[1]->node_type() == Language::reserved_else) {
      key_ptr = EPtr(new Terminal);
      // TODO line num
      key_ptr->expr_type_ = Type::get_bool();
      key_ptr->token_ = "else";
      key_ptr->precedence_ = Language::op_prec.at("MAX");

    } else {
      key_ptr = std::static_pointer_cast<Expression>(nodes[1]);
    }

    auto val_ptr = std::static_pointer_cast<Expression>(nodes[3]);

    pair_list->kv_pairs_.emplace_back(key_ptr, val_ptr);

    return pair_list;
  }

  inline NPtr KVPairList::build_one_assignment_error(NPtrVec&& nodes) {
    error_log.log(nodes[0]->line_num_, "Assignment found where boolean expression was expected. Did you mean `==` instead of `=`?");

    auto assignment_node = std::static_pointer_cast<Assignment>(nodes[0]);

    // TODO this is mostly the same code as Binop::build_operator
    auto binop_ptr = new Binop; 
    // TODO line num
    binop_ptr->lhs_ = assignment_node->lhs_;
    binop_ptr->rhs_ = assignment_node->rhs_;

    binop_ptr->token_ = "==";
    binop_ptr->type_ = Language::generic_operator;
    binop_ptr->precedence_ = Language::op_prec.at("==");

    nodes[0] = NPtr(binop_ptr);


    return build_one(std::forward<NPtrVec>(nodes));
  }

  inline NPtr KVPairList::build_more_assignment_error(NPtrVec&& nodes) {
    error_log.log(nodes[0]->line_num_, "Assignment found where boolean expression was expected. Did you mean `==` instead of `=`?");

    auto assignment_node = std::static_pointer_cast<Assignment>(nodes[1]);

    // TODO this is mostly the same code as Binop::build_operator
    auto binop_ptr = new Binop;
    // TODO line num
    binop_ptr->lhs_ = assignment_node->lhs_;
    binop_ptr->rhs_ = assignment_node->rhs_;

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
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual Type* interpret_as_type() const;
      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);

    private:
      Case() {}
      std::shared_ptr<KVPairList> pairs_;
  };

  inline NPtr Case::build(NPtrVec&& nodes) {
    auto case_ptr = new Case;
    case_ptr->line_num_ = nodes[0]->line_num_;
    case_ptr->pairs_ = std::static_pointer_cast<KVPairList>(nodes[2]);
    return NPtr(case_ptr);
  }




  class Statements : public Node {
    public:
    static NPtr build_one(NPtrVec&& nodes);
    static NPtr build_more(NPtrVec&& nodes);

    virtual std::string to_string(size_t n) const;
    virtual void join_identifiers(Scope* scope);
    virtual void assign_decl_to_scope(Scope* scope);
    virtual void record_dependencies(EPtr) const;
    virtual void verify_types();

    void collect_return_types(std::set<Type*>* return_exprs) const;
    virtual llvm::Value* generate_code(Scope* scope);

    inline size_t size() { return statements_.size(); }

    private:
    Statements() {}
    std::vector<NPtr> statements_;
  };

  inline NPtr Statements::build_one(NPtrVec&& nodes) {
    auto output = new Statements;
    output->statements_.push_back(std::move(nodes[0]));

    return NPtr(output);
  }

  inline NPtr Statements::build_more(NPtrVec&& nodes) {
    auto output = std::static_pointer_cast<Statements>(nodes[0]);
    output->statements_.push_back(std::move(nodes[1]));

    return NPtr(output);
  }


  class FunctionLiteral : public Expression {
    public:
      friend llvm::Value* generate_assignment_code(Scope* scope, EPtr lhs, EPtr rhs);

      static NPtr build(NPtrVec&& nodes) {
        auto fn_lit = new FunctionLiteral;
        fn_lit->line_num_ = nodes[0]->line_num_;

        fn_lit->statements_ = std::static_pointer_cast<Statements>(nodes[2]);

        // TODO scopes inside these statements should point to fn_scope_.

        auto binop_ptr = std::static_pointer_cast<Binop>(nodes[0]);
        fn_lit->return_type_ = std::move(binop_ptr->rhs_);

        // TODO What if the fn_expression is more complicated, like a function
        // of the form (int -> int) -> int? I'm not sure how robust this is
        if (binop_ptr->lhs_->is_declaration()) {
          // TODO This assumes a single declaration, not a comma-separated list
          auto decl_ptr = std::static_pointer_cast<Declaration>(binop_ptr->lhs_);

          fn_lit->inputs_.push_back(decl_ptr);
        }

        return NPtr(fn_lit);
      }

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual Type* interpret_as_type() const;

      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);

      virtual llvm::Function* llvm_function() const { return llvm_function_; }

      virtual ~FunctionLiteral() {}

    private:
      Scope* fn_scope_;
      EPtr return_type_;

      std::vector<DeclPtr> inputs_;
      llvm::Function* llvm_function_;
      std::shared_ptr<Statements> statements_;

      FunctionLiteral() : fn_scope_(ScopeDB::Scope::build()), llvm_function_(nullptr) {}
  };

  
  class While : public Node {
    public:
      static NPtr build(NPtrVec&& nodes) {
        auto while_stmt = new While;
        while_stmt->cond_ = std::static_pointer_cast<Expression>(nodes[1]);
        while_stmt->statements_ = std::static_pointer_cast<Statements>(nodes[3]);
        return NPtr(while_stmt);
      }

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();
      virtual llvm::Value* generate_code(Scope* scope);


    private:
      While() : body_scope_(ScopeDB::Scope::build()) {}

      EPtr cond_;
      std::shared_ptr<Statements> statements_;
      Scope* body_scope_;
  };


}  // namespace AST

#endif  // ICARUS_AST_NODE_H
