#include "ast/terminal.h"

#include "context.h"

namespace AST {
Terminal::Terminal(const TextSpan &span, IR::Val val) : Expression(span) {
  value = std::move(val);
}

void Terminal::assign_scope(Scope *scope) {
  scope_ = scope;
  if (value.type != type::Type_) { return; }
}

type::Type const *Terminal::VerifyType(Context *ctx) {
  ctx->set_type(this, value.type);
  return value.type;
}

Terminal *Terminal::Clone() const { return new Terminal(*this); }
base::vector<IR::Val> Terminal::EmitIR(Context *) { return {value}; }
base::vector<IR::Register> Terminal::EmitLVal(Context *ct) {
  UNREACHABLE(this);
}

}  // namespace AST
