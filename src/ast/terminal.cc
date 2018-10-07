#include "ast/terminal.h"

#include "context.h"

namespace AST {
Terminal::Terminal(const TextSpan &span, IR::Val val) : Expression(span) {
  stage_range_.low = DoneBodyValidationStage;
  value            = std::move(val);
}

void Terminal::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  if (value.type != type::Type_) { return; }
}

type::Type const *Terminal::VerifyType(Context *ctx) {
  ctx->mod_->set_type(ctx->mod_->bound_constants_, this, value.type);
  return value.type;
}

Terminal *Terminal::Clone() const { return new Terminal(*this); }
base::vector<IR::Val> Terminal::EmitIR(Context *) { return {value}; }
base::vector<IR::Register> Terminal::EmitLVal(Context *ct) {
  UNREACHABLE(this);
}

}  // namespace AST
