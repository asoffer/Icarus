#ifndef ICARUS_AST_LITERAL_H
#define ICARUS_AST_LITERAL_H

#include "ast/expression.h"

namespace ast {
// Subtype consisting of any object which can be syntactically verified not to
// be an lvalue. Note that we can't catch all rvalues because, for example,
// identifiers can be lvalues or constants depending on their declaration which
// is elsewhere in the AST.
struct Literal : public Expression {
  Literal(TextSpan const &span = TextSpan()) : Expression(span) {}
  Literal(Literal &&) noexcept      = default;
  Literal(Literal const &) noexcept = default;
  Literal &operator=(Literal &&) noexcept = default;
  Literal &operator=(Literal const &) noexcept = default;
  ~Literal() override {}

  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override {
    UNREACHABLE(*this);
  }
};

} // namespace ast
#endif // ICARUS_AST_LITERAL_H
