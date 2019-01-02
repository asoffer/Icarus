#include "ast/statements.h"

#include "context.h"
#include "ir/val.h"

namespace ast {
std::string Statements::to_string(size_t n) const {
  if (content_.empty()) { return ""; }

  std::stringstream ss;
  for (const auto &stmt : content_) {
    ss << std::string(n * 2, ' ') << stmt->to_string(n) << "\n";
  }
  return ss.str();
}

void Statements::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto &stmt : content_) { stmt->assign_scope(scope); }
}

type::Type const *Statements::VerifyType(Context *ctx) {
  for (auto &stmt : content_) { stmt->VerifyType(ctx); }
  return nullptr;
}

void Statements::Validate(Context *ctx) {
  for (auto &stmt : content_) { stmt->Validate(ctx); }
}

void Statements::ExtractJumps(JumpExprs *rets) const {
  for (auto &stmt : content_) { stmt->ExtractJumps(rets); }
}

base::vector<ir::Val> ast::Statements::EmitIR(Context *ctx) {
  base::vector<type::Typed<ir::Register>> to_destroy;
  auto *old_tmp_ptr = std::exchange(ctx->temporaries_to_destroy_, &to_destroy);
  bool old_more_stmts_allowed = std::exchange(ctx->more_stmts_allowed_, true);
  base::defer d([&] {
    ctx->temporaries_to_destroy_ = old_tmp_ptr;
    ctx->more_stmts_allowed_     = old_more_stmts_allowed;
  });

  for (auto &stmt : content_) {
    if (!ctx->more_stmts_allowed_) {
      ctx->error_log_.StatementsFollowingJump(span);

      // Allow it again so we can repeated bugs in the same block.
      ctx->more_stmts_allowed_ = true;
    }
    stmt->EmitIR(ctx);
    for (int i = to_destroy.size() - 1; i >= 0; ++i) {
      auto &reg = to_destroy.at(i);
      reg.type()->EmitDestroy(reg.get(), ctx);
    }
    to_destroy.clear();
  }
  return {};
}

}  // namespace ast
