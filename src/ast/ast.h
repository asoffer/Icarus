#ifndef ICARUS_AST_AST_H
#define ICARUS_AST_AST_H

#include <algorithm>
#include <map>
#include <memory>
#include <optional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "../base/debug.h"
#include "../frontend/text_span.h"
#include "../ir/val.h"
#include "../operators.h"
#include "../scope.h"
#include "../type/type.h"
#include "bound_constants.h"
#include "dispatch.h"
#include "expression.h"
#include "fn_args.h"

struct Context;

namespace IR {
struct Func;
} // namespace IR

namespace AST {
struct Statements;

// TODO this is hopefully no longer necessary.
#define STAGE_CHECK                                                            \
  do {                                                                         \
    if (stage_range_.high < ThisStage()) { return; }                           \
    if (stage_range_.low >= ThisStage()) { return; }                           \
    stage_range_.low = ThisStage();                                            \
  } while (false)

struct TokenNode : public Node {
  virtual std::string to_string(size_t n) const;

  void SaveReferences(Scope *, std::vector<IR::Val> *) { UNREACHABLE(); }
  TokenNode *Clone() const override;
  virtual void Validate(Context *) {}

  virtual void
  contextualize(const Node *correspondant,
                const std::unordered_map<const Expression *, IR::Val> &);

  virtual IR::Val EmitIR(Context *) { UNREACHABLE(); }

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

  virtual IR::Val EmitIR(Context *);
  IR::Val value = IR::Val::None();
};

struct Identifier : public Expression {
  Identifier() {}
  EXPR_FNS(Identifier);
  Identifier(const TextSpan &span, const std::string &token_string);
  Identifier *Clone() const override;
  virtual IR::Val EmitIR(Context *);
  virtual IR::Val EmitLVal(Context *);

  std::string token;
  Declaration *decl = nullptr;
};

struct Hole : public Terminal {
  Hole() = delete;
  EXPR_FNS(Hole);
  Hole(const TextSpan &span) : Terminal(span, IR::Val::None()) {}
  Hole *Clone() const override;

  IR::Val EmitIR(Context *) override { return IR::Val::None(); }
  IR::Val EmitLVal(Context *) override { return IR::Val::None(); }
};

struct Binop : public Expression {
  EXPR_FNS(Binop);

  Binop *Clone() const override;
  virtual IR::Val EmitIR(Context *);
  virtual IR::Val EmitLVal(Context *);

  Language::Operator op;
  std::unique_ptr<Expression> lhs, rhs;
};

struct Call : public Expression {
  EXPR_FNS(Call);
  Call *Clone() const override;

  IR::Val EmitIR(Context *) override;

  IR::Val EmitOneCallDispatch(
      const std::unordered_map<Expression *, IR::Val *> &expr_map,
      const Binding &binding, Context *ctx);

  std::unique_ptr<Expression> fn_;
  FnArgs<std::unique_ptr<Expression>> args_;

  // Filled in after type verification
  DispatchTable dispatch_table_;
  std::optional<DispatchTable>
  ComputeDispatchTable(std::vector<Expression *> fn_options, Context *ctx);
};

struct Declaration : public Expression {
  EXPR_FNS(Declaration);
  Declaration(bool is_const = false) : const_(is_const) {}
  Declaration(Declaration &&) = default;
  Declaration &operator=(Declaration &&) = default;

  Declaration *Clone() const override;
  IR::Val EmitIR(Context *) override;

  std::unique_ptr<Identifier> identifier;
  std::unique_ptr<Expression> type_expr, init_val;

  // For non-const declarations, holds the address at which the value is being
  // stored. For const values (declared with :: or ::=), holds the actual
  // constant value.
  IR::Val addr = IR::Val::None();

  bool const_ = false;

  // If it's an argument or return value, this points to the function for which
  // it's an argument. Otherwise this field is null.
  // TODO rename this now that it no longer is just for function arguments
  Expression *arg_val = nullptr;

  // These functions are confusingly named. They look correct in normal
  // declarations, but in function arguments, IsDefaultInitialized() is true iff
  // there is no default value provided.
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
  InDecl *Clone() const override;
  std::unique_ptr<Expression> container;
};

struct Unop : public Expression {
  EXPR_FNS(Unop);
  virtual IR::Val EmitIR(Context *);
  virtual IR::Val EmitLVal(Context *);

  Unop *Clone() const override;
  std::unique_ptr<Expression> operand;
  Language::Operator op;
};

struct Access : public Expression {
  EXPR_FNS(Access);
  virtual IR::Val EmitIR(Context *);
  virtual IR::Val EmitLVal(Context *);

  Access *Clone() const override;
  std::string member_name;
  std::unique_ptr<Expression> operand;
};

struct ChainOp : public Expression {
  EXPR_FNS(ChainOp);
  virtual IR::Val EmitIR(Context *);

  ChainOp *Clone() const override;
  std::vector<Language::Operator> ops;
  std::vector<std::unique_ptr<Expression>> exprs;
};

struct CommaList : public Expression {
  CommaList();
  EXPR_FNS(CommaList);

  CommaList *Clone() const override;
  virtual IR::Val EmitIR(Context *);
  virtual IR::Val EmitLVal(Context *);

  std::vector<std::unique_ptr<Expression>> exprs;
};

struct ArrayLiteral : public Expression {
  EXPR_FNS(ArrayLiteral);
  ArrayLiteral *Clone() const override;

  virtual IR::Val EmitIR(Context *);

  std::vector<std::unique_ptr<Expression>> elems;
};

struct ArrayType : public Expression {
  EXPR_FNS(ArrayType);
  virtual IR::Val EmitIR(Context *);
  ArrayType *Clone() const override;

  std::unique_ptr<Expression> length, data_type;
};

struct Case : public Expression {
  EXPR_FNS(Case);
  virtual IR::Val EmitIR(Context *);
  Case *Clone() const override;

  std::vector<
      std::pair<std::unique_ptr<Expression>, std::unique_ptr<Expression>>>
      key_vals;
};

struct FunctionLiteral : public Expression {
  FunctionLiteral() {}
  EXPR_FNS(FunctionLiteral);
  FunctionLiteral(FunctionLiteral &&) = default;
  FunctionLiteral *Clone() const override;

  virtual IR::Val EmitIR(Context *);

  std::unique_ptr<FnScope> fn_scope;
  std::unique_ptr<Expression> return_type_expr;

  std::vector<std::unique_ptr<Declaration>> inputs;
  std::unique_ptr<Statements> statements;

  // Maps the string name of the declared argument to it's index:
  // Example: (a: int, b: char, c: string) -> int
  //           a => 0, b => 1, c => 2
  std::unordered_map<std::string, size_t> lookup_;
  IR::Func *ir_func_ = nullptr;
};

struct GenericFunctionLiteral : public FunctionLiteral {
  GenericFunctionLiteral() {}
  EXPR_FNS(GenericFunctionLiteral);
  GenericFunctionLiteral *Clone() const override;
  IR::Val EmitIR(Context *) override;

  // Attempts to match the call argument types to the dependent types here. If
  // it can it materializes a function literal and returns a pointer to it.
  // Otherwise, returns nullptr.
  std::pair<FunctionLiteral *, Binding>
  ComputeType(const FnArgs<std::unique_ptr<Expression>> &args, Context *ctx);

  // Holds an ordering of the indices of 'inputs' sorted in such a way that if a
  // type depends on a value of another declaration, the dependent type occurs
  // after the type it depends on.
  std::vector<size_t> decl_order_;

  std::map<BoundConstants, FunctionLiteral> fns_;
};

struct For : public Node {
  VIRTUAL_METHODS_FOR_NODES;
  virtual ~For() {}
  For *Clone() const override;

  virtual IR::Val EmitIR(Context *);

  std::vector<std::unique_ptr<InDecl>> iterators;
  std::unique_ptr<Statements> statements;
  std::unique_ptr<ExecScope> for_scope;
};

struct Jump : public Node {
  VIRTUAL_METHODS_FOR_NODES;
  virtual ~Jump() {}
  Jump *Clone() const override;

  enum class JumpType { Restart, Continue, Repeat, Break, Return };

  Jump(const TextSpan &span, JumpType jump_type);

  ExecScope *scope;
  JumpType jump_type;
};

struct ScopeNode : public Expression {
  EXPR_FNS(ScopeNode);
  ScopeNode *Clone() const override;

  virtual IR::Val EmitIR(Context *);

  // If the scope takes an argument, 'expr' is it. Otherwise 'expr' is null
  std::unique_ptr<Expression> expr, scope_expr;
  std::unique_ptr<Statements> stmts;
  std::unique_ptr<ExecScope> internal;
};

struct ScopeLiteral : public Expression {
  EXPR_FNS(ScopeLiteral);
  ScopeLiteral *Clone() const override;

  IR::Val EmitIR(Context *) override;

  std::unique_ptr<Declaration> enter_fn, exit_fn;
  std::unique_ptr<Scope> body_scope;
};

struct StructLiteral : public Expression {
  EXPR_FNS(StructLiteral);
  StructLiteral()                 = default;
  StructLiteral(StructLiteral &&) = default;
  StructLiteral &operator=(StructLiteral &&) = default;
  StructLiteral *Clone() const override;

  IR::Val EmitIR(Context*) override;

  std::unique_ptr<DeclScope> type_scope;
  std::vector<std::unique_ptr<Declaration>> fields_;
};

template <int N> decltype(auto) DoStage(Node *node, Scope *scope, Context *);
template <>
inline decltype(auto) DoStage<0>(Node *node, Scope *scope, Context *) {
  node->assign_scope(scope);
}
template <>
inline decltype(auto) DoStage<1>(Node *node, Scope *, Context *ctx) {
  node->Validate(ctx);
}

template <>
inline decltype(auto) DoStage<2>(Node *node, Scope *, Context *ctx) {
  return node->EmitIR(ctx);
}

template <int Low, int High> struct ApplyStageRange {
  decltype(auto) operator()(Node *node, Scope *scope, Context *ctx) const {
    DoStage<Low>(node, scope, ctx);
    return ApplyStageRange<Low + 1, High>{}(node, scope, ctx);
  }
};
template <int N> struct ApplyStageRange<N, N> {
  decltype(auto) operator()(Node *node, Scope *scope, Context *ctx) const {
    return DoStage<N>(node, scope, ctx);
  }
};
template <int Low, int High>
decltype(auto) DoStages(Node *node, Scope *scope, Context *ctx) {
  return ApplyStageRange<Low, High>{}(node, scope, ctx);
}
} // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS

#endif // ICARUS_AST_AST_H
