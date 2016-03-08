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
#include "AST/DeclType.h"
#include "Type.h"
#include "Scope.h"
#include "ErrorLog.h"
#include "Context.h"

extern ErrorLog error_log;
extern std::queue<std::string> file_queue;
class Scope;
class FnScope;
class CondScope;
class WhileScope;
struct Structure;
struct Enumeration;

template <typename T> T *steal(AST::Expression *&n) {
  // TODO reinterpret_cast for safety in debug mode?
  auto temp = static_cast<T *>(n);
  assert(temp && "stolen pointer is null");
  n = nullptr;
  return temp;
}

template <typename T> T *steal(AST::Node *&n) {
  // TODO reinterpret_cast for safety in debug mode?
  auto temp = static_cast<T *>(n);
  assert(temp && "stolen pointer is null");
  n = nullptr;
  return temp;
}

namespace AST {
using NPtrVec = std::vector<Node *>;

#define ENDING = 0
#define OVERRIDE
#define VIRTUAL_METHODS_FOR_NODES                                              \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void join_identifiers(Scope *scope, bool is_arg = false) ENDING;     \
  virtual void assign_scope() ENDING;                                          \
  virtual void record_dependencies() ENDING;                                   \
  virtual void verify_types() ENDING;                                          \
  virtual std::string graphviz_label() const ENDING;                           \
  virtual Context::Value evaluate(Context &ctx) ENDING;                        \
  virtual llvm::Value *generate_code() ENDING;                                 \
  virtual Time::Eval determine_time() ENDING

#define EXPR_FNS(name, checkname)                                              \
  name();                                                                      \
  virtual ~name();                                                             \
  virtual bool is_##checkname() const OVERRIDE { return true; }                \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void join_identifiers(Scope *scope, bool is_arg = false) ENDING;     \
  virtual void assign_scope() ENDING;                                          \
  virtual void record_dependencies() ENDING;                                   \
  virtual void verify_types() ENDING;                                          \
  virtual std::string graphviz_label() const ENDING;                           \
  virtual llvm::Value *generate_code() ENDING;                                 \
  virtual llvm::Value *generate_lvalue() ENDING;                               \
  virtual Context::Value evaluate(Context &ctx) ENDING;                        \
  virtual Time::Eval determine_time() ENDING;                                  \
  static Node *build(NPtrVec &&nodes)

struct Node {
  Language::NodeType node_type() const { return type_; }
  void set_node_type(Language::NodeType t) { type_ = t; }

  virtual std::string token() const { return token_; }
  void set_token(const std::string &token_string) { token_ = token_string; }

  virtual std::string to_string(size_t n) const;
  virtual void join_identifiers(Scope *scope, bool is_arg = false) {}
  virtual void assign_scope() {}
  virtual void record_dependencies() {}
  virtual void verify_types() {}
  virtual std::string graphviz_label() const;

  virtual Context::Value evaluate(Context &ctx) { return nullptr; }
  virtual llvm::Value *generate_code() { return nullptr; }
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

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  Scope *scope_;

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
  static TokenNode string_literal(size_t line_num, const std::string &str_lit) {
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

    op = Language::is_operator(node_type()) ? Language::lookup_operator.at(tk_)
                                            : Language::Operator::NotAnOperator;
  }

  std::string tk_;
  Language::Operator op;
};

struct Expression : public Node {
  EXPR_FNS(Expression, expression);

  static Node *parenthesize(NPtrVec &&nodes);

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
inline Node *Expression::parenthesize(NPtrVec &&nodes) {
  auto expr_ptr = steal<Expression>(nodes[1]);
  expr_ptr->precedence =
      Language::precedence(Language::Operator::NotAnOperator);
  return expr_ptr;
}

// TODO: This only represents a left unary operator for now
struct Unop : public Expression {
  EXPR_FNS(Unop, unop);

  static Node *build_paren_operator(NPtrVec &&nodes);

  Expression *operand;
  Language::Operator op;
};

struct Access : public Expression {
  EXPR_FNS(Access, access);

  std::string member_name;
  Expression *operand;
};

struct Binop : public Expression {
  EXPR_FNS(Binop, binop);

  static Node *build_operator(NPtrVec &&nodes, Language::Operator op_class);
  static Node *build_paren_operator(NPtrVec &&nodes);
  static Node *build_bracket_operator(NPtrVec &&nodes);
  static Node *build_array_type(NPtrVec &&nodes);

  Language::Operator op;
  Expression *lhs, *rhs;
};

struct ChainOp : public Expression {
  EXPR_FNS(ChainOp, chain_op);

  static Node *join(NPtrVec &&nodes);

  virtual bool is_comma_list() const override {
    return ops.front() == Language::Operator::Comma;
  }

  std::vector<Language::Operator> ops;
  std::vector<Expression *> exprs;
};

struct ArrayLiteral : public Expression {
  EXPR_FNS(ArrayLiteral, array_literal);
  std::vector<Expression *> elems;
};

struct ArrayType : public Expression {
  EXPR_FNS(ArrayType, array_type);

  static Node *build_unknown(NPtrVec &&nodes);

  Expression *length;
  Expression *data_type;
};

struct Terminal : public Expression {
  EXPR_FNS(Terminal, terminal);

  static Node *build(Language::Terminal term_type, NPtrVec &&nodes, Type *t);
  static Node *build_type_literal(NPtrVec &&nodes);
  static Node *build_string_literal(NPtrVec &&nodes);
  static Node *build_true(NPtrVec &&nodes);
  static Node *build_false(NPtrVec &&nodes);
  static Node *build_null(NPtrVec &&nodes);
  static Node *build_int_literal(NPtrVec &&nodes);
  static Node *build_uint_literal(NPtrVec &&nodes);
  static Node *build_real_literal(NPtrVec &&nodes);
  static Node *build_char_literal(NPtrVec &&nodes);
  static Node *build_void_return(NPtrVec &&nodes);
  static Node *build_ASCII(NPtrVec &&nodes);
  static Node *build_alloc(NPtrVec &&nodes);

  Language::Terminal terminal_type;
};

struct Declaration : public Expression {
  EXPR_FNS(Declaration, declaration);

  static Node *build(NPtrVec &&nodes, Language::NodeType node_type, bool infer);
  static Node *build_decl(NPtrVec &&nodes);
  static Node *build_assign(NPtrVec &&nodes);

  // TODO is 'op' necessary? anymore?
  Language::Operator op;
  Identifier *identifier;
  Expression *type_expr;

  DeclType decl_type;
};

struct Assignment : public Binop {
  Assignment() {}
  virtual ~Assignment() {}

  static Node *build(NPtrVec &&nodes);

  virtual std::string to_string(size_t n) const;
  virtual void verify_types();

  virtual llvm::Value *generate_code();
  virtual llvm::Value *generate_lvalue();
  virtual Context::Value evaluate(Context &ctx);
  virtual std::string graphviz_label() const;
};

struct Identifier : public Terminal {
  EXPR_FNS(Identifier, identifier);
  // TODO Identifier() = delete;
  Identifier(size_t line_num, const std::string &token_string);

  llvm::Value *alloc;
  bool is_function_arg;
  Declaration *decl;
};

struct KVPairList : public Node {
  // TODO must have an else. should be stored outside the vector
  static Node *build_one(NPtrVec &&nodes);
  static Node *build_more(NPtrVec &&nodes);
  static Node *build_one_assignment_error(NPtrVec &&nodes);
  static Node *build_more_assignment_error(NPtrVec &&nodes);

  VIRTUAL_METHODS_FOR_NODES;

  virtual Type *verify_types_with_key(Type *key_type);

  inline size_t size() const { return pairs.size(); }

  KVPairList() {}
  ~KVPairList();

  std::vector<std::pair<Expression *, Expression *>> pairs;
};

struct Case : public Expression {
  EXPR_FNS(Case, case);

  KVPairList *kv;
};

struct Statements : public Node {
  static Node *build_one(NPtrVec &&nodes);
  static Node *build_more(NPtrVec &&nodes);
  static Node *build_double_expression_error(NPtrVec &&nodes);
  static Node *build_extra_expression_error(NPtrVec &&nodes);

  VIRTUAL_METHODS_FOR_NODES;

  inline size_t size() { return statements.size(); }
  inline void reserve(size_t n) { return statements.reserve(n); }

  void add_nodes(Statements *stmts) {
    for (auto &stmt : stmts->statements) {
      statements.push_back(std::move(stmt));
    }
  }

  Statements() {}
  virtual ~Statements();

  std::vector<AST::Node *> statements;
};

struct FunctionLiteral : public Expression {
  EXPR_FNS(FunctionLiteral, function_literal);

  FnScope *fn_scope;
  Expression *return_type_expr;

  std::vector<Declaration *> inputs;
  llvm::Function *llvm_fn;
  Statements *statements;
};

struct Conditional : public Node {
  static Node *build_if(NPtrVec &&nodes);
  static Node *build_else_if(NPtrVec &&nodes);
  static Node *build_else(NPtrVec &&nodes);
  static Node *build_extra_else_error(NPtrVec &&nodes);
  static Node *build_extra_else_if_error(NPtrVec &&nodes);
  static Node *build_if_assignment_error(NPtrVec &&nodes);

  VIRTUAL_METHODS_FOR_NODES;

  bool has_else() const { return else_line_num != 0; }

  Conditional() : else_line_num(0) {}
  virtual ~Conditional();

  std::vector<Expression *> conditions;
  std::vector<Statements *> statements;
  std::vector<CondScope *> body_scopes;

  // We use else_line_num to determine if an else branch exists (when it's
  // non-zero) and also for error generation (if multiple else-blocks are
  // present).
  size_t else_line_num;
};

struct For : public Node {
  For();
  virtual ~For();
  VIRTUAL_METHODS_FOR_NODES;

  static Node *build(NPtrVec &&nodes);
  Declaration *iterator;
  Expression *container;
  Statements *statements;

  // TODO change this scope type? Certainly at least change the name from
  // WhileScope to LoopScope
  WhileScope *for_scope;
  // ForScope *for_scope;
};

struct While : public Node {
  While();
  virtual ~While();
  VIRTUAL_METHODS_FOR_NODES;

  static Node *build(NPtrVec &&nodes);
  static Node *build_assignment_error(NPtrVec &&nodes);

  Expression *condition;
  Statements *statements;
  WhileScope *while_scope;
};

struct TypeLiteral : public Expression {
  EXPR_FNS(TypeLiteral, type_literal);

  void build_llvm_internals();

  Structure *type_value;
  Scope *type_scope;
  std::vector<Declaration *> declarations;
};

struct EnumLiteral : public Expression {
  EXPR_FNS(EnumLiteral, enum_literal);

  Enumeration *type_value;
  std::vector<std::string> members;
};

struct Break : public Node {
  static Node *build(NPtrVec &&nodes);
  virtual ~Break();

  virtual std::string to_string(size_t n) const;
  virtual llvm::Value *generate_code();
  virtual Context::Value evaluate(Context &ctx);
  virtual void record_dependencies();
  virtual std::string graphviz_label() const;
  virtual Time::Eval determine_time();

  Break(size_t new_line_num) { line_num = new_line_num; }
};
} // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS
#undef ENDING
#undef OVERRIDE
#endif // ICARUS_AST_NODE_H
