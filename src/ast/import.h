#ifndef ICARUS_AST_IMPORT_H
#define ICARUS_AST_IMPORT_H

#include <memory>
#include <optional>

#include "base/source.h"
#include "ast/expression.h"

namespace AST {
struct Import : public Expression {
  EXPR_FNS(Import);
  virtual IR::Val EmitIR(Context *);
  virtual IR::Val EmitLVal(Context *);

  Import(std::unique_ptr<Expression> expr) : operand_(std::move(expr)) {}

  // TODO optimization: if the operand_ is a string literal, schedule it
  // immediately.
  Import *Clone() const override;
  std::optional<Source::Name> cache_;
  std::unique_ptr<Expression> operand_;
};
} //
#endif  // ICARUS_AST_IMPORT_H
