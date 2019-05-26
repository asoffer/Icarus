#ifndef ICARUS_AST_TERMINAL_H
#define ICARUS_AST_TERMINAL_H

#include <sstream>

#include "ast/expression.h"
#include "ir/block.h"
#include "ir/results.h"
#include "type/primitive.h"

namespace ast {
struct Terminal : public Expression {
  Terminal() = default;
  Terminal(const TextSpan &span, ir::Results results, type::Type const *t)
      : Expression(span), results_(std::move(results)), type_(t) {}
  ~Terminal() override {}

#include "visitor/visitors.xmacro.h"

  ir::Results results_;
  type::Type const *type_;
};
}  // namespace ast

#endif  // ICARUS_AST_TERMINAL_H
