#include "ast/statements.h"

#include "misc/context.h"

namespace ast {
std::string Statements::to_string(size_t n) const {
  if (content_.empty()) { return ""; }

  std::stringstream ss;
  for (const auto &stmt : content_) {
    ss << std::string(n * 2, ' ') << stmt->to_string(n) << "\n";
  }
  return ss.str();
}

void Statements::assign_scope(core::Scope *scope) {
  scope_ = scope;
  for (auto &stmt : content_) { stmt->assign_scope(scope); }
}

void Statements::DependentDecls(DeclDepGraph *g,
                                Declaration *d) const {
  for (auto const &stmt : content_) { stmt->DependentDecls(g, d); }
}

VerifyResult Statements::VerifyType(Context *ctx) {
  for (auto &stmt : content_) { stmt->VerifyType(ctx); }
  return VerifyResult::NonConstant(type::Void());
}

void Statements::ExtractJumps(JumpExprs *rets) const {
  for (auto &stmt : content_) { stmt->ExtractJumps(rets); }
}

void Statements::append(std::unique_ptr<Node> &&node) {
  if (node->is<Statements>()) {
    content_.insert(
        content_.end(),
        std::make_move_iterator(node->as<Statements>().content_.begin()),
        std::make_move_iterator(node->as<Statements>().content_.end()));
  } else {
    content_.push_back(std::move(node));
  }
}

ir::Results ast::Statements::EmitIr(Context *ctx) {
  std::vector<type::Typed<ir::Reg>> to_destroy;
  auto *old_tmp_ptr = std::exchange(ctx->temporaries_to_destroy_, &to_destroy);
  bool old_more_stmts_allowed = std::exchange(ctx->more_stmts_allowed_, true);
  base::defer d([&] {
    ctx->temporaries_to_destroy_ = old_tmp_ptr;
    ctx->more_stmts_allowed_     = old_more_stmts_allowed;
  });

  for (auto &stmt : content_) {
    if (!ctx->more_stmts_allowed_) {
      ctx->error_log()->StatementsFollowingJump(stmt->span);

      // Allow it again so we can repeated bugs in the same block.
      ctx->more_stmts_allowed_ = true;
    }
    stmt->EmitIr(ctx);
    for (int i = static_cast<int>(to_destroy.size()) - 1; i >= 0; --i) {
      auto &reg = to_destroy.at(i);
      reg.type()->EmitDestroy(reg.get(), ctx);
    }
    to_destroy.clear();
  }
  return ir::Results{};
}

}  // namespace ast
