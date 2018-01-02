#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <algorithm>
#include <map>
#include <memory>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "../base/debug.h"
#include "../base/owned_ptr.h"
#include "../base/util.h"
#include "../input/cursor.h"
#include "../ir/ir.h"
#include "../scope.h"
#include "../type/type.h"
#include "dispatch.h"
#include "operators.h"

struct Scope;

enum class Assign : char { Unset, Const, LVal, RVal };

namespace AST {

#define VIRTUAL_METHODS_FOR_NODES                                              \
  virtual std::string to_string(size_t n) const override;                      \
  std::string to_string() const { return to_string(0); }                       \
  virtual void assign_scope(Scope *scope) override;                            \
  virtual void lrvalue_check() override;                                       \
  virtual void verify_types() override;                                        \
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args)        \
      override;                                                                \
  virtual void contextualize(                                                  \
      const Node *correspondant,                                               \
      const std::unordered_map<const Expression *, IR::Val> &) override

#define EXPR_FNS(name)                                                         \
  virtual ~name() {}                                                           \
  virtual std::string to_string(size_t n) const override;                      \
  std::string to_string() const { return to_string(0); }                       \
  virtual void lrvalue_check() override;                                       \
  virtual void assign_scope(Scope *scope) override;                            \
  virtual void verify_types() override;                                        \
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args)        \
      override;                                                                \
  virtual void contextualize(                                                  \
      const Node *correspondant,                                               \
      const std::unordered_map<const Expression *, IR::Val> &) override

struct Node : public base::Cast<Node> {
  virtual std::string to_string(size_t n) const = 0;
  virtual void lrvalue_check() {}
  virtual void assign_scope(Scope *) {}
  virtual void verify_types() {}
  virtual void VerifyReturnTypes(Type *) {}
  virtual IR::Val EmitIR(IR::Cmd::Kind) { NOT_YET(); }
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args) = 0;
  virtual void
  contextualize(const Node *correspondant,
                const std::unordered_map<const Expression *, IR::Val> &) = 0;
  virtual Node *Clone() const = 0;

  std::string to_string() const { return to_string(0); }

  Node(const TextSpan &span = TextSpan()) : span(span) {}
  virtual ~Node() {}

  inline friend std::ostream &operator<<(std::ostream &os, const Node &node) {
    return os << node.to_string(0);
  }

  Scope *scope_ = nullptr;
  TextSpan span;
};

struct Expression : public Node {
  Expression(const TextSpan &span = TextSpan()) : Node(span) {}
  virtual ~Expression(){};
  virtual std::string to_string(size_t n) const = 0;
  virtual void lrvalue_check()                  = 0;
  virtual void assign_scope(Scope *scope)       = 0;
  virtual void verify_types()                   = 0;
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args) = 0;
  std::string to_string() const { return to_string(0); }

  virtual Expression *Clone() const = 0;

  virtual void
  contextualize(const Node *correspondant,
                const std::unordered_map<const Expression *, IR::Val> &) = 0;

  virtual void VerifyReturnTypes(Type *) {}

  virtual IR::Val EmitIR(IR::Cmd::Kind) { NOT_YET(*this); }
  virtual IR::Val EmitLVal(IR::Cmd::Kind) { NOT_YET(*this); }

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

  size_t precedence = std::numeric_limits<size_t>::max();
  Assign lvalue     = Assign::Unset;
  Type *type        = nullptr;
  IR::Val value     = IR::Val::None();
};

struct TokenNode : public Node {
  virtual std::string to_string(size_t n) const;

  void SaveReferences(Scope *, std::vector<IR::Val> *) { UNREACHABLE(); }
  TokenNode *Clone() const override;

  virtual void
  contextualize(const Node *correspondant,
                const std::unordered_map<const Expression *, IR::Val> &);

  virtual IR::Val EmitIR(IR::Cmd::Kind) { UNREACHABLE(); }

  virtual ~TokenNode() {}

  TokenNode(const TextSpan &span = TextSpan(), std::string str = "");

  std::string token;
  Language::Operator op;
};

struct Terminal : public Expression {
  EXPR_FNS(Terminal);
  Terminal() = default;
  Terminal(const TextSpan &span, IR::Val val);

  Terminal *Clone() const override;

  virtual IR::Val EmitIR(IR::Cmd::Kind);
};

struct Identifier : public Terminal {
  Identifier() = delete;
  EXPR_FNS(Identifier);
  Identifier(const TextSpan &span, const std::string &token_string);
  Identifier *Clone() const override;
  virtual IR::Val EmitIR(IR::Cmd::Kind);
  virtual IR::Val EmitLVal(IR::Cmd::Kind);

  std::string token;
  Declaration *decl = nullptr;
};

struct Hole : public Terminal {
  Hole() = delete;
  EXPR_FNS(Hole);
  Hole(const TextSpan &span) : Terminal(span, IR::Val::None()) {}
  Hole *Clone() const override;

  IR::Val EmitIR(IR::Cmd::Kind) override { return IR::Val::None(); }
  IR::Val EmitLVal(IR::Cmd::Kind) override { return IR::Val::None(); }
};

struct Binop : public Expression {
  EXPR_FNS(Binop);
  static base::owned_ptr<Node>
  BuildIndexOperator(std::vector<base::owned_ptr<AST::Node>> nodes);

  Binop *Clone() const override;
  virtual IR::Val EmitIR(IR::Cmd::Kind);
  virtual IR::Val EmitLVal(IR::Cmd::Kind);

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

struct Call : public Expression {
  EXPR_FNS(Call);
  Call *Clone() const override;

  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);

  IR::Val EmitIR(IR::Cmd::Kind) override;

  base::owned_ptr<Expression> fn_;
  std::vector<base::owned_ptr<Expression>> pos_;

  // TODO implement flat map for real
  std::vector<std::pair<std::string, base::owned_ptr<Expression>>> named_;

  // Filled in after type verification
  DispatchTable dispatch_table_;
  DispatchTable ComputeDispatchTable();
};

struct ArgumentMetaData {
  Type *type;
  std::string name;
  bool has_default;
};

struct Declaration : public Expression {
  EXPR_FNS(Declaration);
  Declaration(bool is_const = false) : const_(is_const) {}

  Declaration *Clone() const override;
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes, bool is_const);
  IR::Val EmitIR(IR::Cmd::Kind) override;

  base::owned_ptr<Identifier> identifier;
  base::owned_ptr<Expression> type_expr;
  base::owned_ptr<Expression> init_val;

  // For non-const declarations, holds the address at which the value is being
  // stored. For const values (declared with :: or ::=), holds the actual
  // constant value.
  IR::Val addr = IR::Val::None();

  bool const_ = false;

  // If it's an argument or return value, this points to the
  // function/parametric-struct for which it's an argument. Otherwise this field
  // is null.
  // TODO rename this now that it no longer is just for function arguments
  Expression *arg_val = nullptr;

  inline bool IsInferred() const { return !type_expr; }
  inline bool IsDefaultInitialized() const { return !init_val; }
  inline bool IsCustomInitialized() const {
    return init_val && !init_val->is<Hole>();
  }
  inline bool IsUninitialized() const {
    return init_val && init_val->is<Hole>();
  }
};

struct InDecl : public Declaration {
  EXPR_FNS(InDecl);
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);

  InDecl *Clone() const override;
  base::owned_ptr<Expression> container;
};

struct Statements : public Node {
  static base::owned_ptr<Node>
  build_one(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  build_more(std::vector<base::owned_ptr<AST::Node>> nodes);

  Statements *Clone() const override;
  VIRTUAL_METHODS_FOR_NODES;
  void VerifyReturnTypes(Type *ret_val) override;
  IR::Val EmitIR(IR::Cmd::Kind) override;

  inline size_t size() { return statements.size(); }

  static base::owned_ptr<AST::Statements>
  Merge(std::vector<base::owned_ptr<AST::Statements>> &&stmts_vec) {
    size_t num_stmts = 0;
    for (const auto &stmts : stmts_vec) { num_stmts += stmts->size(); }

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

  CodeBlock *Clone() const override;
  virtual IR::Val EmitIR(IR::Cmd::Kind);
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
  virtual IR::Val EmitIR(IR::Cmd::Kind);
  virtual IR::Val EmitLVal(IR::Cmd::Kind);

  Unop *Clone() const override;
  base::owned_ptr<Expression> operand;
  Language::Operator op;
};

struct Access : public Expression {
  EXPR_FNS(Access);
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR(IR::Cmd::Kind);
  virtual IR::Val EmitLVal(IR::Cmd::Kind);

  Access *Clone() const override;
  std::string member_name;
  base::owned_ptr<Expression> operand;
};

struct ChainOp : public Expression {
  EXPR_FNS(ChainOp);
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR(IR::Cmd::Kind);

  ChainOp *Clone() const override;
  std::vector<Language::Operator> ops;
  std::vector<base::owned_ptr<Expression>> exprs;
};

struct CommaList : public Expression {
  CommaList();
  EXPR_FNS(CommaList);

  CommaList *Clone() const override;
  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR(IR::Cmd::Kind);
  virtual IR::Val EmitLVal(IR::Cmd::Kind);

  std::vector<base::owned_ptr<Expression>> exprs;
};

struct ArrayLiteral : public Expression {
  EXPR_FNS(ArrayLiteral);
  static base::owned_ptr<Node>
  build(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildEmpty(std::vector<base::owned_ptr<AST::Node>> nodes);
  ArrayLiteral *Clone() const override;

  virtual IR::Val EmitIR(IR::Cmd::Kind);

  std::vector<base::owned_ptr<Expression>> elems;
};

struct ArrayType : public Expression {
  EXPR_FNS(ArrayType);
  static base::owned_ptr<Node>
  build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual IR::Val EmitIR(IR::Cmd::Kind);
  ArrayType *Clone() const override;

  base::owned_ptr<Expression> length;
  base::owned_ptr<Expression> data_type;
};

struct Case : public Expression {
  EXPR_FNS(Case);
  virtual IR::Val EmitIR(IR::Cmd::Kind);
  Case *Clone() const override;

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
  FunctionLiteral *Clone() const override;

  virtual IR::Val EmitIR(IR::Cmd::Kind);
  IR::Val EmitIRAndSave(bool, IR::Cmd::Kind);

  // TODO the value of this flag can be passed as a template argument.
  IR::Val EmitTemporaryIR(IR::Cmd::Kind kind);

  base::owned_ptr<FnScope> fn_scope;
  base::owned_ptr<Expression> return_type_expr;

  std::vector<base::owned_ptr<Declaration>> inputs;
  base::owned_ptr<Statements> statements;

  IR::Func *ir_func = nullptr;

  std::unordered_set<Declaration *> captures;

  std::unordered_map<Type *, Declaration *> cache;
};

struct For : public Node {
  virtual ~For() {}
  VIRTUAL_METHODS_FOR_NODES;
  For *Clone() const override;

  virtual IR::Val EmitIR(IR::Cmd::Kind);

  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);

  virtual void VerifyReturnTypes(Type *ret_val) override;

  std::vector<base::owned_ptr<InDecl>> iterators;
  base::owned_ptr<Statements> statements;
  base::owned_ptr<ExecScope> for_scope;
};

struct Jump : public Node {
  enum class JumpType { Restart, Continue, Repeat, Break, Return };
  Jump *Clone() const override;

  virtual void VerifyReturnTypes(Type *ret_val) override;

  static base::owned_ptr<Node>
  build(std::vector<base::owned_ptr<AST::Node>> nodes);
  virtual ~Jump() {}

  VIRTUAL_METHODS_FOR_NODES;

  Jump(const TextSpan &span, JumpType jump_type);

  ExecScope *scope;
  JumpType jump_type;
};

struct ScopeNode : public Expression {
  EXPR_FNS(ScopeNode);
  ScopeNode *Clone() const override;

  static base::owned_ptr<Node>
  Build(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildVoid(std::vector<base::owned_ptr<AST::Node>> nodes);
  static base::owned_ptr<Node>
  BuildScopeNode(base::owned_ptr<Expression> scope_name,
                 base::owned_ptr<Expression> arg_expr,
                 base::owned_ptr<Statements> stmt_node);
  virtual IR::Val EmitIR(IR::Cmd::Kind);

  // If the scope takes an argument, 'expr' is it. Otherwise 'expr' is null
  base::owned_ptr<Expression> expr;
  base::owned_ptr<Expression> scope_expr;
  base::owned_ptr<Statements> stmts;
  base::owned_ptr<ExecScope> internal;
};

struct ScopeLiteral : public Expression {
  ScopeLiteral() = delete;
  EXPR_FNS(ScopeLiteral);
  ScopeLiteral *Clone() const override;

  IR::Val EmitIR(IR::Cmd::Kind) override;

  base::owned_ptr<Declaration> enter_fn;
  base::owned_ptr<Declaration> exit_fn;
  base::owned_ptr<Scope> body_scope;
  ScopeLiteral(const TextSpan &span);
};

} // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS

#endif // ICARUS_AST_AST_H
