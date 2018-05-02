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

struct Module;
struct Context;

#define EXPR_FNS(name)                                                         \
  virtual ~name() {}                                                           \
  virtual std::string to_string(size_t n) const override;                      \
  virtual void assign_scope(Scope *scope) override;                            \
  virtual void ClearIdDecls() override;                                        \
  virtual void VerifyType(Context *) override;                                 \
  virtual void Validate(Context *) override;                                   \
  virtual void SaveReferences(Scope *scope, std::vector<IR::Val> *args)        \
      override;                                                                \
  virtual void ExtractReturns(std::vector<const Expression *> *)               \
      const override;                                                          \
  virtual void contextualize(                                                  \
      const Node *correspondant,                                               \
      const std::unordered_map<const Expression *, IR::Val> &) override


namespace AST {
struct Binop : public Expression {
  EXPR_FNS(Binop);

  Binop *Clone() const override;
  virtual IR::Val EmitIR(Context *);
  virtual IR::Val EmitLVal(Context *);

  Language::Operator op;
  std::unique_ptr<Expression> lhs, rhs;
  DispatchTable dispatch_table_;
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
}  // namespace AST

#undef VIRTUAL_METHODS_FOR_NODES
#undef EXPR_FNS

#endif  // ICARUS_AST_AST_H
