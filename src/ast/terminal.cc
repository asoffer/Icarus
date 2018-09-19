#include "ast/terminal.h"

namespace AST {
Terminal::Terminal(const TextSpan &span, IR::Val val) : Expression(span) {
  stage_range_.low = DoneBodyValidationStage;
  value            = std::move(val);
  type             = value.type;
}

void Terminal::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  if (type != type::Type_) { return; }
}

Terminal *Terminal::Clone() const { return new Terminal(*this); }
base::vector<IR::Val> Terminal::EmitIR(Context *) { return {value}; }
base::vector<IR::Register> Terminal::EmitLVal(Context *ct) {
  UNREACHABLE(this);
}

}  // namespace AST
