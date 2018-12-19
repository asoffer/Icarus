#include "ast/terminal.h"

#include "context.h"

namespace ast {
Terminal::Terminal(const TextSpan &span, ir::Val val) : Expression(span) {
  value = std::move(val);
}

void Terminal::assign_scope(Scope *scope) {
  scope_ = scope;
  if (value.type != type::Type_) { return; }
}

type::Type const *Terminal::VerifyType(Context *ctx) {
  return ctx->set_type(this, value.type);
}

base::vector<ir::Val> Terminal::EmitIR(Context *) { return {value}; }
base::vector<ir::RegisterOr<ir::Addr>> Terminal::EmitLVal(Context *ct) {
  UNREACHABLE(this);
}

}  // namespace ast
