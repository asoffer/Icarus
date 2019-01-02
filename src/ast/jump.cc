#include "ast/jump.h"

#include "ir/cmd.h"
#include "ir/val.h"

namespace ast {
base::vector<ir::Val> Jump::EmitIR(Context *ctx) {
  switch (jump_type) {
    case JumpKind::Return: {
      auto *scope = scope_;
      while (scope != nullptr) {
        scope->MakeAllDestructions(ctx);
        if (scope->is<FnScope>()) { break; }
        scope = scope->parent;
      }
      ctx->more_stmts_allowed_ = false;
      ir::ReturnJump();
    } break;
    case JumpKind::Yield: {
      scope_->MakeAllDestructions(ctx);
      // TODO This isn't correct. There should be a jump of some kind. Or a call
      // to the after handler.
      ctx->yields_stack_.back().clear();
      ctx->more_stmts_allowed_ = false;
      return {};
    } break;
  }
  ir::ReturnJump();
  return {};
}
}  // namespace ast
