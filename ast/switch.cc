#include "ast/switch.h"

#include <numeric>
#include <sstream>

#include "absl/container/flat_hash_set.h"
#include "base/util.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
#include "ir/phi.h"
#include "type/cast.h"
#include "type/pointer.h"
#include "type/type.h"

namespace ast {
std::string Switch::to_string(size_t n) const {
  std::stringstream ss;
  ss << "switch ";
  if (expr_) { ss << "(" << expr_->to_string(n) << ") {\n"; }
  for (const auto &[body, cond] : cases_) {
    ss << std::string((n + 1) * 2, ' ') << body->to_string(n + 1) << " when "
       << cond->to_string(n + 1) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

ir::Results ast::Switch::EmitIr(Context *ctx) {
  absl::flat_hash_map<ir::BlockIndex, ir::Results> phi_args;
  auto land_block = ir::CompiledFn::Current->AddBlock();
  auto *t         = ctx->type_of(this);
  // TODO this is not precisely accurate if you have regular void.
  bool all_paths_jump = (t == type::Void());

  // TODO handle a default value. for now, we're just not checking the very last
  // condition. This is very wrong.

  // TODO handle switching on tuples/multiple values?
  ir::Results expr_results;
  type::Type const *expr_type = nullptr;
  if (expr_) {
    expr_results  = expr_->EmitIr(ctx);
    expr_type = ctx->type_of(expr_.get());
  }

  for (size_t i = 0; i + 1 < cases_.size(); ++i) {
    auto &[body, match_cond] = cases_[i];
    auto expr_block          = ir::CompiledFn::Current->AddBlock();

    ir::Results match_val     = match_cond->EmitIr(ctx);
    ir::RegisterOr<bool> cond = expr_
                                    ? ir::EmitEq(ctx->type_of(match_cond.get()),
                                                 match_val, expr_type, expr_results)
                                    : match_val.get<bool>(0);

    auto next_block = ir::EarlyExitOn<true>(expr_block, cond);

    ir::BasicBlock::Current = expr_block;
    if (body->is<Expression>()) {
      phi_args[ir::BasicBlock::Current] = body->EmitIr(ctx);
      ir::UncondJump(land_block);
    } else {
      // It must be a jump/yield/return, which we've verified in VerifyType.
      body->EmitIr(ctx);
      if (!all_paths_jump) { ctx->more_stmts_allowed_ = true; }
    }

    ir::BasicBlock::Current = next_block;
  }

  if (cases_.back().first->is<Expression>()) {
    phi_args[ir::BasicBlock::Current] = cases_.back().first->EmitIr(ctx);
    ir::UncondJump(land_block);
  } else {
    // It must be a jump/yield/return, which we've verified in VerifyType.
    cases_.back().first->EmitIr(ctx);
    if (!all_paths_jump) { ctx->more_stmts_allowed_ = true; }
  }

  ir::BasicBlock::Current = land_block;
  if (t == type::Void()) {
    return ir::Results{};
  } else {
    return ir::MakePhi(t, ir::Phi(t->is_big() ? type::Ptr(t) : t), phi_args);
  }
}

}  // namespace ast
