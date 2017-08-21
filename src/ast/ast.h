#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <algorithm>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "../base/debug.h"
#include "../base/owned_ptr.h"
#include "../base/util.h"
#include "../cursor.h"
#include "../ir/ir.h"
#include "../scope.h"
#include "../type/type.h"
#include "operators.h"

struct Scope;

enum class Assign : char { Unset, Const, LVal, RVal };

namespace AST {

#define ENDING = 0
#define OVERRIDE
#define VIRTUAL_METHODS_FOR_NODES                                              \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void assign_scope(Scope *scope) ENDING;                              \
  virtual void lrvalue_check() ENDING;                                         \
  virtual void verify_types() ENDING;                                          \
  virtual void contextualize(Scope *scope, std::vector<IR::Val> *args) ENDING; \
  virtual base::owned_ptr<AST::Node> contextualize(                            \
      const std::unordered_map<const Expression *, IR::Val> &) const ENDING

#define EXPR_FNS(name)                                                         \
  virtual ~name(){};                                                           \
  virtual std::string to_string(size_t n) const ENDING;                        \
  virtual void lrvalue_check() ENDING;                                         \
  virtual void assign_scope(Scope *scope) ENDING;                              \
  virtual void verify_types() ENDING;                                          \
  virtual void contextualize(Scope *scope, std::vector<IR::Val> *args) ENDING; \
  virtual base::owned_ptr<AST::Node> contextualize(                            \
      const std::unordered_map<const Expression *, IR::Val> &) const ENDING;   \
  base::owned_ptr<name> copy_stub() const

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const = 0;
  virtual void lrvalue_check() {}
  virtual void assign_scope(Scope *) {}
  virtual void verify_types() {}
  virtual void VerifyReturnTypes(Type *) {}
  virtual IR::Val EmitIR() { NOT_YET(); }
  virtual void contextualize(Scope *scope, std::vector<IR::Val> *args) = 0;
  virtual base::owned_ptr<AST::Node> contextualize(
      const std::unordered_map<const Expression *, IR::Val> &) const = 0;

  virtual bool is_hole() const { return false; }

  Node(Cursor cursor = Cursor()) : loc(cursor) {}
  virtual ~Node() {}

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  Scope *scope_ = nullptr;
  Cursor loc;
};

struct Expression : public Node {
  Expression();
  virtual ~Expression(){};
  virtual std::string to_string(size_t n) const = 0;
  virtual void lrvalue_check()                  = 0;
  virtual void assign_scope(Scope *scope)       = 0;
  virtual void verify_types()                   = 0;
  virtual void contextualize(Scope *scope, std::vector<IR::Val> *args) = 0;

  // TOD make this method pure abstract
  virtual void GenerateRequirements() const { NOT_YET(); }

  virtual base::owned_ptr<AST::Node> contextualize(
      const std::unordered_map<const Expression *, IR::Val> &) const = 0;

  virtual void VerifyReturnTypes(Type *) {}

  virtual IR::Val EmitIR() { NOT_YET(*this); }
  virtual IR::Val EmitLVal() { NOT_YET(*this); }

  // Use these two functions to verify that an identifier can be declared using
  // these expressions. We pass in a string representing the identifier being
  // declared to be used in error messages.
  //
  // VerifyTypeForDeclaration verifies that the expresison represents a type and
  // returns the type it represents (or Error if the type is invalid). An
  // expression could be invalid if it doesn't represent a type or it represnts
  // void or parametric struct.
  Type *VerifyTypeForDeclaration(const std::string &id_tok);

  // VerifyValueForDeclaration verifies that the expression's type can be used
  // for a declaration. In practice, it is typically used on initial values for
  // a declaration. That is, when we see "foo := bar", we verify that the type
  // of bar is valid. This function has the same return characteristics as
  // VerifyTypeForDeclaration. Specifically, it returns the type or Error if the
  // type is invalid.
  Type *VerifyValueForDeclaration(const std::string &id_tok);

  size_t precedence;
  Assign lvalue;
  Type *type = nullptr;
  IR::Val value;
};

struct TokenNode : public Node {
  virtual std::string to_string(size_t n) const;

  void contextualize(Scope *, std::vector<IR::Val> *) { UNREACHABLE(); }
  virtual void GenerateRequirements() const { UNREACHABLE(); }

  virtual base::owned_ptr<Node>
  contextualize(const std::unordered_map<const Expression *, IR::Val> &) const {
    UNREACHABLE();
  }

  virtual IR::Val EmitIR() { UNREACHABLE(); }

  virtual ~TokenNode() {}

  TokenNode(const Cursor &cursor = Cursor(), std::string str = "");

  std::string token;
  Language::Operator op;
};

#undef ENDING
#define ENDING override
#undef OVERRIDE
#define OVERRIDE override

struct Terminal : public Expression {
  EXPR_FNS(Terminal);
  Terminal() = default;
  Terminal(const Cursor &cursor, IR::Val val);

  virtual IR::Val EmitIR();
  virtual void GenerateRequirements() const {}
  virtual bool is_hole() const override { return value == IR::Val::None(); }
};

struct Identifier : public Terminal {
  Identifier() = delete;
  EXPR_FNS(Identifier);
  Identifier(const Cursor &cursor, const std::string &token_string);
  virtual IR::Val EmitIR();
  virtual IR::Val EmitLVal();
  virtual bool is_hole() const override { return false; }
  virtual void GenerateRequirements() const {}

  std::string token;
  Declaration *decl = nullptr;
};

struct Binop : public Expression {
  EXPR_FNS(Binop);
  static base::owned_ptr<Node>
  BuildCallOperator(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildIndexOperator(std::vector<base::owned_ptr<AST::Node>> nodes);

  virtual IR::Val EmitIR();
  virtual IR::Val EmitLVal();

  bool is_assignment() const {
    return op == Language::Operator::Assign || op == Language::Operator::OrEq ||
           op == Language::Operator::XorEq || op == Language::Operator::AndEq ||
           op == Language::Operator::AddEq || op == Language::Operator::SubEq ||
           op == Language::Operator::MulEq || op == Language::Operator::DivEq ||
           op == Language::Operator::ModEq;
  }

  Language::Operator op;
  base::owned_ptr<Expression> lhs, rhs;
};

struct Declaration : public Expression {
  EXPR_FNS(Declaration);
  Declaration(bool is_const = false) : const_(is_const) {}

  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes, bool is_const);
  IR::Val EmitIR() override;

  base::owned_ptr<Identifier> identifier;
  base::owned_ptr<Expression> type_expr;
  base::owned_ptr<Expression> init_val;

  // For non-const declarations, holds the address at which the value is being
  // stored. For const values (declared with :: or ::=), holds the actual
  // constant value.
  IR::Val addr = IR::Val::None();

  bool const_ = false;

  // If it's an argument, this points to the function/parametric-struct for
  // which it's an argument. Otherwise this field is null.
  Expression *arg_val = nullptr;

  inline bool IsInferred() const { return !type_expr; }
  inline bool IsDefaultInitialized() const { return !init_val; }
  inline bool IsCustomInitialized() const {
    return init_val && !init_val->is_hole();
  }
  inline bool IsUninitialized() const {
    return init_val && init_val->is_hole();
  }
};

struct Generic : public Declaration {
  EXPR_FNS(Generic);
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);

  base::owned_ptr<Expression> test_fn;
};

struct InDecl : public Declaration {
  EXPR_FNS(InDecl);
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);

  base::owned_ptr<Expression> container;
};

struct Statements : public Node {
  static base::owned_ptr<Node>
  build_one(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  build_more(std::vector<base::owned_ptr<AST::Node>> nodes);

  VIRTUAL_METHODS_FOR_NODES;
  base::owned_ptr<Statements> copy_stub() const;
  void VerifyReturnTypes(Type *ret_val) override;
  IR::Val EmitIR() override;

  inline size_t size() { return statements.size(); }

  static base::owned_ptr<AST::Statements>
  Merge(std::vector<AST::Statements *> stmts_vec) {
    size_t num_stmts = 0;
    for (const auto stmts : stmts_vec) { num_stmts += stmts->size(); }

    auto result = base::make_owned<AST::Statements>();
    result->statements.reserve(num_stmts);

    for (auto &stmts : stmts_vec) {
      for (auto &stmt : stmts->statements) {
        result->statements.push_back(std::move(stmt));
      }
    }

    return result;
  }

  Statements() {}
  virtual ~Statements() {}

  std::vector<base::owned_ptr<AST::Node>> statements;
};

struct CodeBlock : public Expression {
  EXPR_FNS(CodeBlock);
  base::owned_ptr<Statements> stmts;
  std::string error_message; // To be used if stmts == nullptr

  virtual IR::Val EmitIR();
  static base::owned_ptr<Node>
  BuildFromStatementsSameLineEnd(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildEmpty(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildFromStatements(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildFromOneStatement(std::vector<base::owned_ptr<AST::Node>> nodes);
};

struct Unop : public Expression {
  EXPR_FNS(Unop);
  static base::owned_ptr<Node>
  BuildLeft(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildDots(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual void VerifyReturnTypes(Type *ret_val) override;
  virtual IR::Val EmitIR();
  virtual IR::Val EmitLVal();

  base::owned_ptr<Expression> operand;
  Language::Operator op;
};

struct Access : public Expression {
  EXPR_FNS(Access);
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR();
  virtual IR::Val EmitLVal();

  std::string member_name;
  base::owned_ptr<Expression> operand;
};

struct ChainOp : public Expression {
  EXPR_FNS(ChainOp);
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR();

  virtual void GenerateRequirements() const;

  std::vector<Language::Operator> ops;
  std::vector<base::owned_ptr<Expression>> exprs;
};

struct CommaList : public Expression {
  CommaList();
  EXPR_FNS(CommaList);

  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR();
  virtual IR::Val EmitLVal();

  std::vector<base::owned_ptr<Expression>> exprs;
};

struct ArrayLiteral : public Expression {
  EXPR_FNS(ArrayLiteral);
  static base::owned_ptr<Node>
  build(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildEmpty(std::vector<base::owned_ptr<AST::Node>> nodes);

  virtual IR::Val EmitIR();

  std::vector<base::owned_ptr<Expression>> elems;
};

struct ArrayType : public Expression {
  EXPR_FNS(ArrayType);
  static base::owned_ptr<Node>
  build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR();

  base::owned_ptr<Expression> length;
  base::owned_ptr<Expression> data_type;
};

struct Case : public Expression {
  EXPR_FNS(Case);
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR();

  std::vector<
      std::pair<base::owned_ptr<Expression>, base::owned_ptr<Expression>>>
      key_vals;
};

struct FunctionLiteral : public Expression {
  FunctionLiteral() {}
  EXPR_FNS(FunctionLiteral);
  static base::owned_ptr<Node>
  build(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildOneLiner(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildNoLiner(std::vector<base::owned_ptr<AST::Node>> nodes);

  virtual IR::Val EmitIR();

  base::owned_ptr<FnScope> fn_scope;
  base::owned_ptr<Expression> return_type_expr;

  std::vector<base::owned_ptr<Declaration>> inputs;
  base::owned_ptr<Statements> statements;

  IR::Func ir_func;

  std::unordered_set<Declaration *> captures;

  std::unordered_map<Type *, Declaration *> cache;
};

struct For : public Node {
  virtual ~For() {}
  VIRTUAL_METHODS_FOR_NODES;
  base::owned_ptr<For> copy_stub() const;

  virtual IR::Val EmitIR();

  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);

  virtual void VerifyReturnTypes(Type *ret_val) override;

  std::vector<base::owned_ptr<InDecl>> iterators;
  base::owned_ptr<Statements> statements;
  base::owned_ptr<ExecScope> for_scope;
};

struct Jump : public Node {
  enum class JumpType { Restart, Continue, Repeat, Break, Return };

  virtual void VerifyReturnTypes(Type *ret_val) override;

  static base::owned_ptr<Node>
  build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual ~Jump() {}

  VIRTUAL_METHODS_FOR_NODES;
  base::owned_ptr<Jump> copy_stub() const;

  Jump(const Cursor &new_cursor, JumpType jump_type);

  ExecScope *scope;
  JumpType jump_type;
};

struct ScopeNode : public Expression {
  EXPR_FNS(ScopeNode);

  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildVoid(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildScopeNode(base::owned_ptr<Expression> scope_name,
                 base::owned_ptr<Expression> arg_expr,
                 base::owned_ptr<Statements> stmt_node);
  virtual IR::Val EmitIR();

  // If the scope takes an argument, 'expr' is it. Otherwise 'expr' is null
  base::owned_ptr<Expression> expr;
  base::owned_ptr<Expression> scope_expr;
  base::owned_ptr<Statements> stmts;
  base::owned_ptr<ExecScope> internal;
};

struct ScopeLiteral : public Expression {
  ScopeLiteral() = delete;
  EXPR_FNS(ScopeLiteral);

  IR::Val EmitIR() override;

  base::owned_ptr<Declaration> enter_fn;
  base::owned_ptr<Declaration> exit_fn;
  base::owned_ptr<Scope> body_scope;
  ScopeLiteral(const Cursor &cursor);
};

} // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS
#undef ENDING
#undef OVERRIDE

#endif // ICARUS_AST_AST_H
