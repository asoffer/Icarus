#ifndef ICARUS_AST_H
#define ICARUS_AST_H

#include <string>
#include <iostream>
#include <map>
#include <set>
#include <vector>
#include <queue>

#include "Language.h"
#include "TimeEval.h"
#include "Type.h"
#include "typedefs.h"
#include "Scope.h"
#include "ErrorLog.h"
#include "Context.h"

extern ErrorLog error_log;
extern std::queue<std::string> file_queue;
class Scope;
class FnScope;
class CondScope;
class WhileScope;
class Structure;
class Enumeration;

namespace AST {
#define ENDING = 0
#define VIRTUAL_METHODS_FOR_NODES                                          \
  virtual std::string to_string(size_t n) const                    ENDING; \
  virtual void join_identifiers(Scope* scope, bool is_arg = false) ENDING; \
  virtual void assign_decl_to_scope(Scope* scope)                  ENDING; \
  virtual void record_dependencies()                               ENDING; \
  virtual void verify_types()                                      ENDING; \
  virtual Context::Value evaluate(Context& ctx)                    ENDING; \
  virtual llvm::Value* generate_code(Scope* scope)                 ENDING; \
  virtual Time::Eval determine_time()                              ENDING

#define VIRTUAL_METHODS_FOR_EXPRESSION                                     \
  virtual std::string to_string(size_t n) const                    ENDING; \
  virtual void join_identifiers(Scope* scope, bool is_arg = false) ENDING; \
  virtual void assign_decl_to_scope(Scope* scope)                  ENDING; \
  virtual void record_dependencies()                               ENDING; \
  virtual void verify_types()                                      ENDING; \
  virtual Type* interpret_as_type()                                ENDING; \
  virtual llvm::Value* generate_code(Scope* scope)                 ENDING; \
  virtual llvm::Value* generate_lvalue(Scope* scope)               ENDING; \
  virtual Context::Value evaluate(Context& ctx)                    ENDING; \
  virtual Time::Eval determine_time()                              ENDING

  class Node {
    public:
      Language::NodeType node_type() const { return type_; }
      void set_node_type(Language::NodeType t) { type_ = t; }

      virtual std::string token() const { return token_; }
      void set_token(const std::string& token_string) {
        token_ = token_string;
      }
      size_t line_num() const { return line_num_; }

      virtual std::string to_string(size_t n) const;
      virtual void join_identifiers(Scope* scope, bool is_arg = false) {}
      virtual void assign_decl_to_scope(Scope* scope) {}
      virtual void record_dependencies() {}
      virtual void verify_types() {}

      virtual Context::Value evaluate(Context& ctx) { return nullptr; }
      virtual llvm::Value* generate_code(Scope* scope) { return nullptr; }
      virtual Time::Eval determine_time() { return Time::error; }

      Time::Eval time() { return time_; }

      virtual bool is_identifier()    const { return type_ == Language::identifier; }
      virtual bool is_terminal()      const { return false; }
      virtual bool is_expression()    const { return false; }
      virtual bool is_binop()         const { return false; }
      virtual bool is_chain_op()      const { return false; }
      virtual bool is_comma_list()    const { return false; }
      virtual bool is_declaration()   const { return false; }
      virtual bool is_array_type()    const { return false; }
      virtual bool is_type_literal()  const { return false; }
      virtual bool is_enum_literal()  const { return false; }
      virtual bool is_array_literal() const { return false; }
      virtual bool is_token_node()    const { return false; }

      Node(size_t line_num = 0, Language::NodeType type = Language::unknown, const std::string& token = "")
        : type_(type), token_(token), line_num_(line_num), time_(Time::error) {}

      virtual ~Node(){}

      inline friend std::ostream& operator<<(std::ostream& os, const Node& node) {
        return os << node.to_string(0);
      }

    protected:
      Language::NodeType type_;
      std::string token_;
      size_t line_num_;
      Time::Eval time_;
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
      Expression();
      virtual ~Expression(){}
      virtual bool is_expression() const { return true; }
      VIRTUAL_METHODS_FOR_EXPRESSION;

      friend class KVPairList;
      friend class Binop;
      friend class Declaration;
      friend class ::Scope;

      static NPtr parenthesize(NPtrVec&& nodes);

      size_t precedence() const { return precedence_; }

      virtual Type* type() const { return expr_type_; }
      virtual bool is_literal(Type* t) const {
        return is_terminal() && !is_identifier() && type() == t;
      }

      llvm::Value* llvm_value(Context::Value v);
    
    protected:
      size_t precedence_;
      Type* expr_type_;
  };

#undef ENDING
#define ENDING

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

      VIRTUAL_METHODS_FOR_EXPRESSION;

    private:
      EPtr expr_;
      Language::Operator op_;
  };


  class Binop : public Expression {
    public:
      friend class FunctionLiteral;
      friend class ::ErrorLog;

      static NPtr build_operator(NPtrVec&& nodes, Language::Operator op_class);
      static NPtr build(NPtrVec&& nodes);
      static NPtr build_paren_operator(NPtrVec&& nodes);
      static NPtr build_bracket_operator(NPtrVec&& nodes);
      static NPtr build_array_type(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION;

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
      friend class FunctionLiteral;
      friend class ArrayType;
      friend class ArrayLiteral;
      friend class Binop;

      static NPtr build(NPtrVec&& nodes);
      static NPtr join(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION;

      virtual bool is_chain_op() const { return true; }
      virtual bool is_comma_list() const { return ops_.front() == Language::Operator::Comma; }

    private:
      std::vector<Language::Operator> ops_;
      std::vector<EPtr> exprs_;
  };


  class ArrayLiteral : public Expression {
    public:
      friend class ChainOp;

      virtual bool is_array_literal() const { return true; }

      static NPtr build(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION;

    private:
      std::vector<EPtr> elems_;
  };

  class ArrayType : public Expression {
    public:
      static NPtr build(NPtrVec&& nodes);
      static NPtr build_unknown(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION;

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
      static NPtr build(Language::Terminal term_type, NPtrVec&& nodes, Type* t);
      static NPtr build_type_literal(NPtrVec&& nodes);
      static NPtr build_string_literal(NPtrVec&& nodes);
      static NPtr build_true(NPtrVec&& nodes);
      static NPtr build_false(NPtrVec&& nodes);
      static NPtr build_int_literal(NPtrVec&& nodes);
      static NPtr build_uint_literal(NPtrVec&& nodes);
      static NPtr build_real_literal(NPtrVec&& nodes);
      static NPtr build_char_literal(NPtrVec&& nodes);
      static NPtr build_void_return(NPtrVec&& nodes);
      static NPtr build_ASCII(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_EXPRESSION;

      virtual bool is_terminal() const { return true; }


      Terminal() {}

    private:
      Language::Terminal terminal_type_;
  };

  class Assignment : public Binop {
    public:
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
      Identifier() = delete;
      Identifier(size_t line_num, const std::string& token_string);
      virtual ~Identifier() {}
      virtual bool is_identifier() const { return true; }
      static NPtr build(NPtrVec&& nodes);
      VIRTUAL_METHODS_FOR_EXPRESSION;

      llvm::Value* alloc_;
      bool is_function_arg_;      
  };

  inline NPtr Identifier::build(NPtrVec&& nodes) {
    if (nodes[0]->token() == "string") {
      file_queue.emplace("lib/string.ic");
    }

    return std::make_shared<Identifier>(nodes[0]->line_num(), nodes[0]->token());
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
      friend class Assignment;
      friend class ::Scope;

      static NPtr build(NPtrVec&& nodes, Language::NodeType node_type, bool infer);
      static NPtr build_decl(NPtrVec&& nodes);
      static NPtr build_assign(NPtrVec&& nodes);

      inline std::string identifier_string() const { return id_->token(); }
      inline IdPtr declared_identifier() const { return id_; }
      inline EPtr declared_type() const { return decl_type_; }

      VIRTUAL_METHODS_FOR_EXPRESSION;

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
      friend class Case;

      // TODO must have an else. should be stored outside the vector
      static NPtr build_one(NPtrVec&& nodes);
      static NPtr build_more(NPtrVec&& nodes);
      static NPtr build_one_assignment_error(NPtrVec&& nodes);
      static NPtr build_more_assignment_error(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_NODES;

      virtual Type* verify_types_with_key(Type* key_type);

      inline size_t size() const { return kv_pairs_.size(); }

      KVPairList() {}

    private:
      std::vector<std::pair<EPtr, EPtr>> kv_pairs_;
  };


  class Case : public Expression {
    public:
      static NPtr build(NPtrVec&& nodes);

      Case() {}
      virtual ~Case() {}

      VIRTUAL_METHODS_FOR_EXPRESSION;

    private:
      std::shared_ptr<KVPairList> pairs_;
  };

  inline NPtr Case::build(NPtrVec&& nodes) {
    auto case_ptr = std::make_shared<Case>();
    case_ptr->line_num_ = nodes[0]->line_num();
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

      VIRTUAL_METHODS_FOR_NODES;

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

  class FunctionLiteral : public Expression {
    public:
      FunctionLiteral();
      virtual ~FunctionLiteral() {}
      static NPtr build(NPtrVec&& nodes);
      VIRTUAL_METHODS_FOR_EXPRESSION;

      friend class Binop;
      friend llvm::Value* generate_assignment_code(Scope* scope, EPtr lhs, EPtr rhs);

      virtual llvm::Function* llvm_function() const { return llvm_function_; }

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

      VIRTUAL_METHODS_FOR_NODES;

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

  class While : public Node {
    public:
      While();
      virtual ~While() {}

      static NPtr build(NPtrVec&& nodes);
      static NPtr build_assignment_error(NPtrVec&& nodes);

      VIRTUAL_METHODS_FOR_NODES;

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
      TypeLiteral();
      virtual ~TypeLiteral() {}
      virtual bool is_type_literal() const { return true; }
      static NPtr build(NPtrVec&& nodes);
      VIRTUAL_METHODS_FOR_EXPRESSION;

      void build_llvm_internals();
      friend class Declaration;

    private:
      Scope* type_scope_;
      Structure* type_value_;
      std::vector<DeclPtr> decls_;
  };

//#define AST_EXPR_CLASS(camel, underscore) \
//  class camel : public Expression
//  AST_EXPR_CLASS(EnumLiteral, enum_literal) {
  class EnumLiteral : public Expression {
    public:

      EnumLiteral();
      virtual ~EnumLiteral() {}
      virtual bool is_enum_literal() const { return true; }
      static NPtr build(NPtrVec&& nodes);
      VIRTUAL_METHODS_FOR_EXPRESSION;

      friend class ::Enumeration;
      friend class Declaration;

    private:
      Scope* enum_scope_;
      Enumeration* type_value_;
      std::vector<std::string> vals_;
  };


  class Break : public Node {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual llvm::Value* generate_code(Scope* scope);
      virtual Context::Value evaluate(Context& ctx);

      virtual Time::Eval determine_time();

      Break(size_t line_num) {
        line_num_ = line_num;
      }

  };

  inline NPtr Break::build(NPtrVec&& nodes) {
    return std::make_shared<Break>(nodes[0]->line_num());
  }


}  // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef VIRTUAL_METHODS_FOR_EXPRESSION
#undef ENDING

#endif  // ICARUS_AST_NODE_H
