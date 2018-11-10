#include "ast/switch.h"

#include <numeric>
#include <sstream>
#include <unordered_set>
#include "ast/verify_macros.h"
#include "base/util.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "type/pointer.h"
#include "type/type.h"

namespace AST {
std::string Switch::to_string(size_t n) const {
  std::stringstream ss;
  ss << "switch {\n";
  for (const auto & [ expr, cond ] : cases_) {
    ss << std::string((n + 1) * 2, ' ') << expr->to_string(n + 1) << " when ";
    ss << std::string((n + 1) * 2, ' ') << cond->to_string(n + 1) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void Switch::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto & [ expr, cond ] : cases_) {
    expr->assign_scope(scope);
    cond->assign_scope(scope);
  }
}

type::Type const *Switch::VerifyType(Context *ctx) {
  std::unordered_set<const type::Type *> types;
  for (auto & [ expr, cond ] : cases_) {
    auto *cond_type = cond->VerifyType(ctx);
    auto *expr_type = expr->VerifyType(ctx);
    if (cond_type != type::Bool) { NOT_YET("handle type error"); }
    // TODO if there's an error, an unorderded_set is not helpful for giving
    // good error messages.
    types.insert(expr_type);
  }
  if (types.empty()) { NOT_YET("handle type error"); }
  auto *t =
      std::accumulate(types.begin(), types.end(), *types.begin(), type::Join);
  if (t == nullptr) {
    NOT_YET("handle type error");
    return nullptr;
  }
  ctx->set_type(this, t);
  return t;
}

void Switch::Validate(Context *ctx) {
  for (auto & [ expr, cond ] : cases_) {
    expr->Validate(ctx);
    cond->Validate(ctx);
  }
}

void Switch::ExtractJumps(JumpExprs *rets) const {
  for (auto & [ expr, cond ] : cases_) {
    expr->ExtractJumps(rets);
    cond->ExtractJumps(rets);
  }
}

base::vector<IR::Val> AST::Switch::EmitIR(Context *ctx) {
  base::unordered_map<IR::BlockIndex, IR::Val> phi_args;
  auto land_block = IR::Func::Current->AddBlock();

  // TODO handle a default value. for now, we're just not checking the very last
  // condition. This is very wrong.
  for (size_t i = 0; i < cases_.size() - 1; ++i) {
    auto & [ expr, cond ] = cases_[i];
    auto expr_block       = IR::Func::Current->AddBlock();
    auto next_block =
        IR::EarlyExitOn<true>(expr_block, cond->EmitIR(ctx)[0].reg_or<bool>());

    IR::BasicBlock::Current           = expr_block;
    auto val                          = expr->EmitIR(ctx)[0];
    phi_args[IR::BasicBlock::Current] = std::move(val);
    IR::UncondJump(land_block);

    IR::BasicBlock::Current = next_block;
  }

  phi_args[IR::BasicBlock::Current] = cases_.back().first->EmitIR(ctx)[0];
  IR::UncondJump(land_block);

  IR::BasicBlock::Current = land_block;
  auto *t                 = ctx->type_of(this);
  return {IR::MakePhi(IR::Phi(t->is_big() ? type::Ptr(t) : t), phi_args)};
}

base::vector<IR::Register> AST::Switch::EmitLVal(Context *ctx) {
  UNREACHABLE(*this);
}

}  // namespace AST
