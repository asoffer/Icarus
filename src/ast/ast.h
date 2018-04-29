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
#include "../frontend/operators.h"
#include "../frontend/text_span.h"
#include "../ir/val.h"
#include "../scope.h"
#include "bound_constants.h"
#include "dispatch.h"
#include "expression.h"
#include "fn_args.h"
#include "unop.h"

struct Module;
struct Context;

namespace IR {
struct Func;
}  // namespace IR

namespace AST {
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

struct Binop : public Expression {
  EXPR_FNS(Binop);

  Binop *Clone() const override;
  virtual IR::Val EmitIR(Context *);
  virtual IR::Val EmitLVal(Context *);

  Language::Operator op;
  std::unique_ptr<Expression> lhs, rhs;
  DispatchTable dispatch_table_;
};

struct Call : public Expression {
  EXPR_FNS(Call);
  Call *Clone() const override;

  IR::Val EmitIR(Context *) override;

  std::unique_ptr<Expression> fn_;
  FnArgs<std::unique_ptr<Expression>> args_;

  // Filled in after type verification
  DispatchTable dispatch_table_;
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
  bool IsCustomInitialized() const;
  bool IsUninitialized() const;
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
  std::vector<DispatchTable> dispatch_tables_;
};

struct CommaList : public Expression {
  CommaList() = default;
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

struct FunctionLiteral : public Expression {
  FunctionLiteral() {}
  EXPR_FNS(FunctionLiteral);
  FunctionLiteral(FunctionLiteral &&) = default;
  FunctionLiteral *Clone() const override;

  void CompleteBody(Module *mod);
  virtual IR::Val EmitIR(Context *);

  std::unique_ptr<FnScope> fn_scope;

  std::vector<std::unique_ptr<Declaration>> inputs;
  std::vector<std::unique_ptr<Expression>> outputs;
  std::unique_ptr<Statements> statements;

  // Maps the string name of the declared argument to it's index:
  // Example: (a: int, b: char, c: string) -> int
  //           a => 0, b => 1, c => 2
  std::unordered_map<std::string, size_t> lookup_;
  IR::Func *ir_func_                     = nullptr;
  const BoundConstants *bound_constants_ = nullptr;
  bool completed_                        = false;
  bool return_type_inferred_             = true;
};

struct GenericFunctionLiteral : public FunctionLiteral {
  GenericFunctionLiteral();
  EXPR_FNS(GenericFunctionLiteral);
  GenericFunctionLiteral *Clone() const override;
  IR::Val EmitIR(Context *) override;

  // Attempts to match the call argument types to the dependent types here. If
  // it can it materializes a function literal and returns a pointer to it.
  // Otherwise, returns nullptr.
  std::pair<FunctionLiteral *, Binding> ComputeType(
      const FnArgs<Expression *> &args, Context *ctx);

  // Holds an ordering of the indices of 'inputs' sorted in such a way that if a
  // type depends on a value of another declaration, the dependent type occurs
  // after the type it depends on.
  std::vector<size_t> decl_order_;

  std::map<BoundConstants, FunctionLiteral> fns_;
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

  IR::Val EmitIR(Context *) override;

  std::unique_ptr<DeclScope> type_scope;
  std::vector<std::unique_ptr<Declaration>> fields_;
};
}  // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS

#endif  // ICARUS_AST_AST_H
