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
#include "Scope.h"
#include "ErrorLog.h"
#include "Context.h"

extern ErrorLog error_log;

namespace AST {
#define ENDING = 0;
#define VIRTUAL_METHODS_FOR_EXPRESSION                            \
  virtual std::string to_string(size_t n) const      ENDING \
  virtual void join_identifiers(Scope* scope)        ENDING \
  virtual void assign_decl_to_scope(Scope* scope)    ENDING \
  virtual void record_dependencies(EPtr eptr) const  ENDING \
  virtual void verify_types()                        ENDING \
  virtual Type* interpret_as_type()                  ENDING \
  virtual llvm::Value* generate_code(Scope* scope)   ENDING \
  virtual llvm::Value* generate_lvalue(Scope* scope) ENDING \
  virtual Context::Value evaluate(Context& ctx)      ENDING \

  class Node {
    public:
      friend class ::Scope;
      friend class KVPairList;
      friend class Expression;
      friend class Terminal;
      friend class Identifier;
      friend class Break;
      friend class Unop;
      friend class FunctionLiteral;
      friend class Case;
      friend class Binop;
      friend class ChainOp;
      friend class Declaration;
      friend class Statements;
      friend class Assignment;
      friend class ArrayType;
      friend class ArrayLiteral;
      friend class TypeLiteral;
      friend class EnumLiteral;

      Language::NodeType node_type() const { return type_; }
      void set_node_type(Language::NodeType t) { type_ = t; }

      virtual std::string token() const { return token_; }
      void set_token(const std::string& token_string) {
        token_ = token_string;
      }
      size_t line_num() const { return line_num_; }

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope) {}
      virtual void assign_decl_to_scope(Scope* scope) {}
      virtual void record_dependencies(EPtr eptr) const {}
      virtual void verify_types() {}

      virtual Context::Value evaluate(Context& ctx) { return nullptr; }
      virtual llvm::Value* generate_code(Scope* scope) { return nullptr; }

      bool is_return() const {
        return node_type() == Language::return_expression;
      }
      bool is_print() const {
        return node_type() == Language::print_expression;
      }

      virtual bool is_identifier() const { return type_ == Language::identifier; }
      virtual bool is_terminal() const { return false; }
      virtual bool is_expression() const { return false; }
      virtual bool is_binop() const { return false; }
      virtual bool is_chain_op() const { return false; }
      virtual bool is_comma_list() const { return false; }
      virtual bool is_declaration() const { return false; }
      virtual bool is_array_type() const { return false; }
      virtual bool is_type_literal() const { return false; }
      virtual bool is_enum_literal() const { return false; }
      virtual bool is_array_literal() const { return false; }
      virtual bool is_token_node() const { return false; }

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

  class TokenNode : public Node {
    public:
      static TokenNode eof(size_t line_num) {
        return TokenNode(line_num, Language::eof);
      }

      static TokenNode newline() {
        return TokenNode(0, Language::newline);
      }

      static TokenNode string_literal(size_t line_num, const std::string& str_lit) {
        return TokenNode(line_num, Language::string_literal, str_lit);
      }

      Language::Operator operator_type() const { return op_; }

      virtual bool is_token_node() const { return true; }
      virtual std::string token() const {
        return tk_;
      }

      virtual ~TokenNode() {}

      // TODO make newline default a bof (beginning of file)
      TokenNode(size_t line_num = 0,
          Language::NodeType in_node_type = Language::newline,
          std::string str_lit = "")
        : Node(line_num, in_node_type), tk_(std::move(str_lit)) {

          if (Language::is_operator(node_type())) {
            op_ = Language::lookup_operator.at(tk_);
          } else {
            op_ = Language::Operator::NotAnOperator;
          }
        }

    private:
      std::string tk_;
      Language::Operator op_;
  };



  class Expression : public Node {
    public:
      friend class ::Scope;
      friend class KVPairList;
      friend class Unop;
      friend class Binop;
      friend class ChainOp;
      friend class Declaration;
      friend class Assignment;
      friend class ArrayLiteral;
      friend class ArrayType;
      friend class Conditional;

      static NPtr parenthesize(NPtrVec&& nodes);

      size_t precedence() const { return precedence_; }

      VIRTUAL_METHODS_FOR_EXPRESSION

        virtual Type* type() const { return expr_type_; }
      virtual bool is_literal(Type* t) const {
        return is_terminal() && !is_identifier() && type() == t;
      }

      llvm::Value* llvm_value(Context::Value v);

      virtual bool is_expression() const { return true; }


      virtual ~Expression(){}

      Expression() : expr_type_(Type::get_unknown()) {}

    protected:
      size_t precedence_;
      Type* expr_type_;
  };

#undef ENDING
#define ENDING ;

  inline NPtr Expression::parenthesize(NPtrVec&& nodes) {
    auto expr_ptr = std::static_pointer_cast<Expression>(nodes[1]);
    expr_ptr->precedence_ =
      Language::precedence(Language::Operator::NotAnOperator);
    return expr_ptr;
  }

  // TODO: This only represents a left unary operator for now
  class Unop : public Expression {
    public:
      friend class Statements;

      static NPtr build(NPtrVec&& nodes);
      static NPtr build_paren_operator(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION

    private:
        EPtr expr_;
        Language::Operator op_;
  };


  class Binop : public Expression {
    public:
      friend class KVPairList;
      friend class FunctionLiteral;
      friend class Conditional;
      friend class ::ErrorLog;

      static NPtr build_operator(NPtrVec&& nodes, Language::Operator op_class);
      static NPtr build(NPtrVec&& nodes);
      static NPtr build_paren_operator(NPtrVec&& nodes);
      static NPtr build_bracket_operator(NPtrVec&& nodes);
      static NPtr build_array_type(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION

        virtual bool is_binop() const { return true; }


      virtual ~Binop(){}

      Binop() {}

    protected:
      Language::Operator op_;
      EPtr lhs_;
      EPtr rhs_;
  };


  class ChainOp : public Expression {
    public:
      friend class ArrayType;
      friend class ArrayLiteral;
      friend class FunctionLiteral;
      friend class Binop;
      friend class ::ErrorLog;

      static NPtr build(NPtrVec&& nodes);
      static NPtr join(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual Type* interpret_as_type();

      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);

      virtual bool is_chain_op() const { return true; }
      virtual bool is_comma_list() const { return ops_.front() == Language::Operator::Comma; }

    private:
      std::vector<Language::Operator> ops_;
      std::vector<EPtr> exprs_;
  };


  class ArrayLiteral : public Expression {
    public:
      virtual bool is_array_literal() const { return true; }

      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual Type* interpret_as_type();
      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);

    private:
      std::vector<EPtr> elems_;
  };

  class ArrayType : public Expression {
    public:
      friend class ::Scope;
      friend class Identifier;
      friend class Declaration;

      static NPtr build(NPtrVec&& nodes);
      static NPtr build_unknown(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual Type* interpret_as_type();
      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);

      virtual bool is_array_type() const { return true; }

      EPtr length() const { return len_; }
      EPtr data_type() const { return array_type_; }

      ArrayType() {}

    private:
      EPtr len_;
      EPtr array_type_;
  };


  class Terminal : public Expression {
    public:
      friend class KVPairList;

      static NPtr build(Language::Terminal term_type, NPtrVec&& nodes, Type* t);
      static NPtr build_type_literal(NPtrVec&& nodes);
      static NPtr build_string_literal(NPtrVec&& nodes);
      static NPtr build_true(NPtrVec&& nodes);
      static NPtr build_false(NPtrVec&& nodes);
      static NPtr build_integer_literal(NPtrVec&& nodes);
      static NPtr build_unsigned_integer_literal(NPtrVec&& nodes);
      static NPtr build_real_literal(NPtrVec&& nodes);
      static NPtr build_character_literal(NPtrVec&& nodes);
      static NPtr build_void_return(NPtrVec&& nodes);
      static NPtr build_ASCII(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope) {}
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();

      virtual bool is_terminal() const { return true; }

      virtual Type* interpret_as_type();
      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);

      Terminal() {}

    private:
      Language::Terminal terminal_type_;
  };

  class Assignment : public Binop {
    public:
      friend class ::ErrorLog;

      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void verify_types();

      virtual llvm::Value* generate_code(Scope* scope);
      virtual llvm::Value* generate_lvalue(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);

      Assignment() {}
      virtual ~Assignment(){}
  };

  class Identifier : public Terminal, public std::enable_shared_from_this<Identifier> {

    public:
      friend class Assignment;
      static NPtr build(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION

        virtual bool is_identifier() const { return true; }

      Identifier(size_t line_num, const std::string& token_string) : alloc_(nullptr) {
        token_ = token_string;
        expr_type_ = Type::get_unknown();
        precedence_ =
          Language::precedence(Language::Operator::NotAnOperator);
        line_num_ = line_num;
      }

      llvm::Value* alloc_;
  };

  inline NPtr Identifier::build(NPtrVec&& nodes) {
    return std::make_shared<Identifier>(nodes[0]->line_num_, nodes[0]->token());
  }

  // class Declaration
  //
  // Represents declarations that may or may not be made with type inference.
  // Either of these can be represented.
  //
  // identifier : type
  // identifer := value
  class Declaration : public Expression {
    public:
      friend class ::Scope;
      friend class Assignment;

      static NPtr build(NPtrVec&& nodes, Language::NodeType node_type, bool infer);
      static NPtr build_decl(NPtrVec&& nodes);
      static NPtr build_assign(NPtrVec&& nodes);

      inline std::string identifier_string() const { return id_->token(); }
      inline IdPtr declared_identifier() const { return id_; }
      inline EPtr declared_type() const { return decl_type_; }

      VIRTUAL_METHODS_FOR_EXPRESSION

        bool type_is_inferred() const { return infer_type_; } 

      virtual bool is_declaration() const { return true; }

      Declaration() {}
      virtual ~Declaration(){}

    private:
      // The identifier being declared
      IdPtr id_;

      // May represent the declared type or the value whose type is being
      // inferred
      Language::Operator op_;
      EPtr decl_type_;
      Scope* scope_;
      bool infer_type_;
  };


  class KVPairList : public Node {
    public:
      // TODO must have an else. should be stored outside the vector
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
      virtual Context::Value evaluate(Context& ctx);

      inline size_t size() const { return kv_pairs_.size(); }

      KVPairList() {}

    private:
      std::vector<std::pair<EPtr, EPtr>> kv_pairs_;
  };


  class Case : public Expression {
    public:
      static NPtr build(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION

        Case() {}
      virtual ~Case() {}

    private:
      std::shared_ptr<KVPairList> pairs_;
  };

  inline NPtr Case::build(NPtrVec&& nodes) {
    auto case_ptr = std::make_shared<Case>();
    case_ptr->line_num_ = nodes[0]->line_num_;
    case_ptr->pairs_ = std::static_pointer_cast<KVPairList>(nodes[2]);
    return case_ptr;
  }


  class Statements : public Node {
    public:
      friend class TypeLiteral;
      friend class EnumLiteral;

      static NPtr build_one(NPtrVec&& nodes);
      static NPtr build_more(NPtrVec&& nodes);
      static NPtr build_double_expression_error(NPtrVec&& nodes);
      static NPtr build_extra_expression_error(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr) const;
      virtual void verify_types();

      void collect_return_types(std::set<Type*>* return_exprs) const;
      virtual llvm::Value* generate_code(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);

      inline size_t size() { return statements_.size(); }
      inline void reserve(size_t n) { return statements_.reserve(n); }

      void add_nodes(StmtsPtr stmts) {
        for (auto& stmt : stmts->statements_) {
          statements_.push_back(std::move(stmt));
        }
      }

      Statements() {}
      virtual ~Statements() {}

    private:
      std::vector<NPtr> statements_;
  };

  inline NPtr Statements::build_one(NPtrVec&& nodes) {
    auto output = std::make_shared<Statements>();
    output->statements_.push_back(std::move(nodes[0]));

    return output;
  }

  inline NPtr Statements::build_more(NPtrVec&& nodes) {
    auto output = std::static_pointer_cast<Statements>(nodes[0]);
    output->statements_.push_back(std::move(nodes[1]));

    return output;
  }

  inline NPtr Statements::build_double_expression_error(NPtrVec&& nodes) {
    error_log.log(nodes[0]->line_num_, "Adjacent expressions");

    auto output = std::make_shared<Statements>();
    output->line_num_ = nodes[0]->line_num_;
    output->statements_.push_back(std::move(nodes[0]));
    output->statements_.push_back(std::move(nodes[1]));

    return output;
  }

  inline NPtr Statements::build_extra_expression_error(NPtrVec&& nodes) {
    error_log.log(nodes[0]->line_num_, "Adjacent expressions");

    auto output = std::static_pointer_cast<Statements>(nodes[0]);
    output->statements_.push_back(std::move(nodes[1]));

    return output;
  }

  class FunctionLiteral : public Expression {
    public:
      friend llvm::Value* generate_assignment_code(Scope* scope, EPtr lhs, EPtr rhs);
      friend class Binop;

      static NPtr build(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION

        virtual llvm::Function* llvm_function() const { return llvm_function_; }

      FunctionLiteral() : fn_scope_(new FnScope), llvm_function_(nullptr) {}
      virtual ~FunctionLiteral() {}

    private:
      FnScope* fn_scope_;
      EPtr return_type_;

      std::vector<DeclPtr> inputs_;
      llvm::Function* llvm_function_;
      std::shared_ptr<Statements> statements_;

  };


  class Conditional : public Node {
    public:
      static NPtr build_if(NPtrVec&& nodes);
      static NPtr build_else_if(NPtrVec&& nodes);
      static NPtr build_else(NPtrVec&& nodes);
      static NPtr build_extra_else_error(NPtrVec&& nodes);
      static NPtr build_extra_else_if_error(NPtrVec&& nodes);
      static NPtr build_if_assignment_error(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();
      virtual llvm::Value* generate_code(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);

      bool has_else() const { return else_line_num_ != 0; }

      Conditional() : else_line_num_(0) {}
      virtual ~Conditional() {}
    private:

      std::vector<EPtr> conds_;
      std::vector<std::shared_ptr<Statements>> statements_;
      std::vector<CondScope*> body_scopes_;

      // We use else_line_num_ to determine if an else branch exists (when it's
      // non-zero) and also for error generation (if multiple else-blocks are
      // present).
      size_t else_line_num_;
  };

  inline NPtr Conditional::build_if(NPtrVec&& nodes) {
    auto if_stmt = std::make_shared<Conditional>();
    if_stmt->conds_ = { std::static_pointer_cast<Expression>(nodes[1]) };
    if_stmt->statements_ = { std::static_pointer_cast<Statements>(nodes[3]) };
    if_stmt->body_scopes_.push_back(Scope::build<CondScope>());
    return if_stmt;
  }

  inline NPtr Conditional::build_extra_else_error(NPtrVec&& nodes) {
    auto if_stmt = std::static_pointer_cast<Conditional>(nodes[0]);
    error_log.log(nodes[1]->line_num(), "If-statement already has an else-branch. The first else-branch is on line " + std::to_string(if_stmt->else_line_num_) + ".");

    return std::move(nodes[0]);
  }

  inline NPtr Conditional::build_extra_else_if_error(NPtrVec&& nodes) {
    auto if_stmt = std::static_pointer_cast<Conditional>(nodes[0]);
    error_log.log(nodes[1]->line_num(), "Else-if block is unreachable because it follows an else block. The else-block is on line " + std::to_string(if_stmt->else_line_num_) + ".");

    return std::move(nodes[0]);
  }

  inline NPtr Conditional::build_else_if(NPtrVec&& nodes) {
    auto if_stmt = std::static_pointer_cast<Conditional>(std::move(nodes[0]));
    auto else_if = std::static_pointer_cast<Conditional>(std::move(nodes[2]));

#ifdef DEBUG
    if (else_if->conds_.size()       != 1 ||
        else_if->statements_.size()  != 1 ||
        else_if->body_scopes_.size() != 1) {
      std::cerr << "FATAL: Else-if statement constructed by parser with multiple conditional blocks." << std::endl;
    }
#endif

    if_stmt->conds_.push_back(std::move(else_if->conds_.front()));
    if_stmt->statements_.push_back(std::move(else_if->statements_.front()));
    if_stmt->body_scopes_.push_back(Scope::build<CondScope>());
    return if_stmt;
  }

  inline NPtr Conditional::build_else(NPtrVec&& nodes) {
    auto if_stmt = std::static_pointer_cast<Conditional>(std::move(nodes[0]));
    if_stmt->else_line_num_ = nodes[1]->line_num();
    if_stmt->statements_.push_back(
        std::static_pointer_cast<Statements>(std::move(nodes[3])));
    if_stmt->body_scopes_.push_back(Scope::build<CondScope>());
    return std::move(if_stmt);
  }

  inline NPtr Conditional::build_if_assignment_error(NPtrVec&& nodes) {
    nodes[1] = error_log.assignment_vs_equality(nodes[1]);
    return build_if(std::forward<NPtrVec&&>(nodes));
  }

  class While : public Node {
    public:
      static NPtr build(NPtrVec&& nodes);
      static NPtr build_assignment_error(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope);
      virtual void assign_decl_to_scope(Scope* scope);
      virtual void record_dependencies(EPtr eptr) const;
      virtual void verify_types();
      virtual llvm::Value* generate_code(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);


      While() : body_scope_(Scope::build<WhileScope>()) {}
      virtual ~While() {}

    private:
      EPtr cond_;
      std::shared_ptr<Statements> statements_;
      WhileScope* body_scope_;
  };

  inline NPtr While::build(NPtrVec&& nodes) {
    auto while_stmt = std::make_shared<While>();
    while_stmt->cond_ = std::static_pointer_cast<Expression>(nodes[1]);
    while_stmt->statements_ = std::static_pointer_cast<Statements>(nodes[3]);
    return while_stmt;
  }

  inline NPtr While::build_assignment_error(NPtrVec&& nodes) {
    nodes[1] = error_log.assignment_vs_equality(nodes[1]);
    return build(std::forward<NPtrVec&&>(nodes));
  }

  class TypeLiteral : public Expression {
    public:
      friend class Declaration;

      static NPtr build(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION

        virtual bool is_type_literal() const { return true; }

      TypeLiteral() :
        type_scope_(Scope::build<TypeScope>()), type_value_(nullptr) {}
      virtual ~TypeLiteral() {}

    private:
      Scope* type_scope_;
      Type* type_value_;
      std::vector<DeclPtr> decls_;
  };


  class EnumLiteral : public Expression {
    public:

      static NPtr build(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION

        virtual bool is_enum_literal() const { return true; }

      // TODO Will TypeScope suffice?
      EnumLiteral() :
        enum_scope_(Scope::build<TypeScope>()), type_value_(nullptr) {}
      virtual ~EnumLiteral() {}

    private:
      Scope* enum_scope_;
      Type* type_value_;
      std::vector<IdPtr> vals_;
  };

  inline NPtr EnumLiteral::build(NPtrVec&& nodes) {
    auto enum_lit_ptr = std::make_shared<EnumLiteral>();
    enum_lit_ptr->line_num_ = nodes[0]->line_num_;
    enum_lit_ptr->expr_type_ = Type::get_type();

    auto stmts = std::static_pointer_cast<Statements>(std::move(nodes[2]));
    for (auto&& stmt : stmts->statements_) {
      // TODO we ignore everything that isn't a declaration.
      // This is a cheap way to get started, but probably not ideal.
      if (!stmt->is_identifier()) continue;

      auto decl = std::static_pointer_cast<Identifier>(std::move(stmt));
      enum_lit_ptr->vals_.emplace_back(std::move(decl));
    }

    return enum_lit_ptr;
  }


  class Break : public Node {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual llvm::Value* generate_code(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);

      Break(size_t line_num) {
        line_num_ = line_num;
      }

  };

  inline NPtr Break::build(NPtrVec&& nodes) {
    return std::make_shared<Break>(nodes[0]->line_num_);
  }


}  // namespace AST

#undef VIRTUAL_METHODS_FOR_EXPRESSION
#undef ENDING

#endif  // ICARUS_AST_NODE_H
