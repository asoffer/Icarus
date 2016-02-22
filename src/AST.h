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
#define OVERRIDE
#define VIRTUAL_METHODS_FOR_NODES                                              \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void join_identifiers(Scope *scope, bool is_arg = false) ENDING;     \
  virtual void assign_scope(Scope *scope) ENDING;                              \
  virtual void record_dependencies() ENDING;                                   \
  virtual void verify_types() ENDING;                                          \
  virtual std::string graphviz_label() const ENDING;                           \
  virtual Context::Value evaluate(Context &ctx) ENDING;                        \
  virtual llvm::Value *generate_code(Scope *scope) ENDING;                     \
  virtual Time::Eval determine_time() ENDING

#define EXPR_FNS(name, checkname)                                              \
  name();                                                                      \
  virtual ~name() {}                                                           \
  virtual bool is_##checkname() const OVERRIDE { return true; }                \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void join_identifiers(Scope *scope, bool is_arg = false) ENDING;     \
  virtual void assign_scope(Scope *scope) ENDING;                              \
  virtual void record_dependencies() ENDING;                                   \
  virtual void verify_types() ENDING;                                          \
  virtual std::string graphviz_label() const ENDING;                           \
  virtual llvm::Value *generate_code(Scope *scope) ENDING;                     \
  virtual llvm::Value *generate_lvalue(Scope *scope) ENDING;                   \
  virtual Context::Value evaluate(Context &ctx) ENDING;                        \
  virtual Time::Eval determine_time() ENDING;                                  \
  static NPtr build(NPtrVec &&nodes)

  struct Node {
    Language::NodeType node_type() const { return type_; }
    void set_node_type(Language::NodeType t) { type_ = t; }

    virtual std::string token() const { return token_; }
    void set_token(const std::string& token_string) {
      token_ = token_string;
    }

    virtual std::string to_string(size_t n) const;
    virtual void join_identifiers(Scope* scope, bool is_arg = false) {}
    virtual void assign_scope(Scope* scope) {}
    virtual void record_dependencies() {}
    virtual void verify_types() {}
    virtual std::string graphviz_label() const;

    virtual Context::Value evaluate(Context& ctx) { return nullptr; }
    virtual llvm::Value* generate_code(Scope* scope) { return nullptr; }
    virtual Time::Eval determine_time() { return Time::error; }

    Time::Eval time() { return time_; }

    virtual bool is_identifier() const { return false; }
    virtual bool is_terminal() const { return false; }
    virtual bool is_expression() const { return false; }
    virtual bool is_binop() const { return false; }
    virtual bool is_function_literal() const { return false; }
    virtual bool is_chain_op() const { return false; }
    virtual bool is_case() const { return false; }
    virtual bool is_unop() const { return false; }
    virtual bool is_access() const { return false; }
    virtual bool is_comma_list() const { return false; }
    virtual bool is_declaration() const { return false; }
    virtual bool is_array_type() const { return false; }
    virtual bool is_type_literal() const { return false; }
    virtual bool is_enum_literal() const { return false; }
    virtual bool is_array_literal() const { return false; }
    virtual bool is_token_node() const { return false; }

    Node(size_t line_num = 0, Language::NodeType type = Language::unknown,
         const std::string &token = "")
        : scope_(nullptr), type_(type), token_(token), line_num(line_num),
          time_(Time::error) {}

    virtual ~Node() {}

    friend struct Access;

    inline friend std::ostream& operator<<(std::ostream& os, const Node& node) {
      return os << node.to_string(0);
    }

    Scope* scope_;

    Language::NodeType type_;
    std::string token_;
    size_t line_num;
    Time::Eval time_;
  };

  struct TokenNode : public Node {
    static TokenNode eof(size_t line_num) {
      return TokenNode(line_num, Language::eof);
    }

    static TokenNode newline() { return TokenNode(0, Language::newline); }
    static TokenNode string_literal(size_t line_num,
                                    const std::string &str_lit) {
      return TokenNode(line_num, Language::string_literal, str_lit);
    }

    virtual bool is_token_node() const { return true; }
    virtual std::string token() const { return tk_; }

    virtual ~TokenNode() {}

    // TODO make newline default a bof (beginning of file)
    TokenNode(size_t line_num = 0,
              Language::NodeType in_node_type = Language::newline,
              std::string str_lit = "")
        : Node(line_num, in_node_type), tk_(std::move(str_lit)) {

      op = Language::is_operator(node_type())
               ? Language::lookup_operator.at(tk_)
               : Language::Operator::NotAnOperator;
    }

    std::string tk_;
    Language::Operator op;
  };

  struct Expression : public Node {
    EXPR_FNS(Expression, expression);

    static NPtr parenthesize(NPtrVec &&nodes);

    virtual bool is_literal(Type *t) const {
      return is_terminal() && !is_identifier() && type == t;
    }

    llvm::Value *llvm_value(Context::Value v);

    size_t precedence;
    Type *type;
  };

#undef ENDING
#define ENDING override
#undef OVERRIDE
#define OVERRIDE override
  inline NPtr Expression::parenthesize(NPtrVec&& nodes) {
    auto expr_ptr = std::static_pointer_cast<Expression>(nodes[1]);
    expr_ptr->precedence =
        Language::precedence(Language::Operator::NotAnOperator);
    return expr_ptr;
  }

  // TODO: This only represents a left unary operator for now
  struct Unop : public Expression {
    EXPR_FNS(Unop, unop);

    static NPtr build_paren_operator(NPtrVec&& nodes);

    EPtr operand;
    Language::Operator op;
  };

  struct Access : public Expression {
    EXPR_FNS(Access, access);

    std::string member_name;
    EPtr operand;
  };

  struct Binop : public Expression {
    EXPR_FNS(Binop, binop);

    static NPtr build_operator(NPtrVec &&nodes, Language::Operator op_class);
    static NPtr build_paren_operator(NPtrVec &&nodes);
    static NPtr build_bracket_operator(NPtrVec &&nodes);
    static NPtr build_array_type(NPtrVec &&nodes);

    Language::Operator op;
    EPtr lhs, rhs;
  };

  struct ChainOp : public Expression {
    EXPR_FNS(ChainOp, chain_op);

    static NPtr join(NPtrVec&& nodes);

    virtual bool is_comma_list() const override {
      return ops.front() == Language::Operator::Comma;
    }

    std::vector<Language::Operator> ops;
    std::vector<EPtr> exprs;
  };

  struct ArrayLiteral : public Expression {
    EXPR_FNS(ArrayLiteral, array_literal);
    std::vector<EPtr> elems;
  };

  struct ArrayType : public Expression {
    EXPR_FNS(ArrayType, array_type);

    static NPtr build_unknown(NPtrVec &&nodes);

    EPtr length;
    EPtr data_type;
  };

  struct Terminal : public Expression {
    EXPR_FNS(Terminal, terminal);

    static NPtr build(Language::Terminal term_type, NPtrVec&& nodes, Type* t);
    static NPtr build_type_literal(NPtrVec&& nodes);
    static NPtr build_string_literal(NPtrVec&& nodes);
    static NPtr build_true(NPtrVec&& nodes);
    static NPtr build_false(NPtrVec&& nodes);
    static NPtr build_null(NPtrVec&& nodes);
    static NPtr build_int_literal(NPtrVec&& nodes);
    static NPtr build_uint_literal(NPtrVec&& nodes);
    static NPtr build_real_literal(NPtrVec&& nodes);
    static NPtr build_char_literal(NPtrVec&& nodes);
    static NPtr build_void_return(NPtrVec&& nodes);
    static NPtr build_ASCII(NPtrVec&& nodes);
    static NPtr build_alloc(NPtrVec&& nodes);

    Language::Terminal terminal_type;
  };

  struct Assignment : public Binop {
    Assignment() {}
    virtual ~Assignment(){}

    static NPtr build(NPtrVec&& nodes);

    virtual std::string to_string(size_t n) const;
    virtual void verify_types();

    virtual llvm::Value* generate_code(Scope* scope);
    virtual llvm::Value* generate_lvalue(Scope* scope);
    virtual Context::Value evaluate(Context& ctx);
    virtual std::string graphviz_label() const;
  };

  struct Identifier : public Terminal, public std::enable_shared_from_this<Identifier> {
    EXPR_FNS(Identifier, identifier);
    // TODO Identifier() = delete;
    Identifier(size_t line_num, const std::string& token_string);

    llvm::Value* alloc;
    bool is_function_arg;
    Declaration* decl;
  };

  struct Declaration : public Expression {
    EXPR_FNS(Declaration, declaration);

    static NPtr build(NPtrVec &&nodes, Language::NodeType node_type,
                      bool infer);
    static NPtr build_decl(NPtrVec &&nodes);
    static NPtr build_assign(NPtrVec &&nodes);

    Language::Operator op;
    IdPtr identifier;
    EPtr type_expr;
    bool is_inferred;
  };

  struct KVPairList : public Node {
    // TODO must have an else. should be stored outside the vector
    static NPtr build_one(NPtrVec&& nodes);
    static NPtr build_more(NPtrVec&& nodes);
    static NPtr build_one_assignment_error(NPtrVec&& nodes);
    static NPtr build_more_assignment_error(NPtrVec&& nodes);

    VIRTUAL_METHODS_FOR_NODES;

    virtual Type* verify_types_with_key(Type* key_type);

    inline size_t size() const { return pairs.size(); }

    KVPairList() {}

    std::vector<std::pair<EPtr, EPtr>> pairs;
  };


  class Case : public Expression {
    public:
      EXPR_FNS(Case, case);

    private:
      std::shared_ptr<KVPairList> pairs_;
  };

  inline NPtr Case::build(NPtrVec&& nodes) {
    auto case_ptr = std::make_shared<Case>();
    case_ptr->line_num = nodes[0]->line_num;
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
      EXPR_FNS(FunctionLiteral, function_literal);

      virtual llvm::Function* llvm_function() const { return llvm_function_; }

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

  class TypeLiteral : public Expression {
    public:
      EXPR_FNS(TypeLiteral, type_literal);

      void build_llvm_internals();
      friend struct Declaration;

      Structure* type_value_;

    private:
      Scope* type_scope_;  // TODO replace with general scope_
      std::vector<DeclPtr> decls_;
  };

  class EnumLiteral : public Expression {
    public:
      EXPR_FNS(EnumLiteral, enum_literal);

      friend class ::Enumeration;
      friend struct Declaration;

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
      virtual void record_dependencies();
      virtual std::string graphviz_label() const;
      virtual Time::Eval determine_time();

      Break(size_t new_line_num) {
        line_num = new_line_num;
      }

  };
}  // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS
#undef ENDING
#undef OVERRIDE
#endif  // ICARUS_AST_NODE_H
