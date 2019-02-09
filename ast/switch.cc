#include "ast/switch.h"

#include <numeric>
#include <sstream>
#include <unordered_set>

#include "base/util.h"
#include "ir/components.h"
#include "ir/func.h"
#include "ir/phi.h"
#include "type/cast.h"
#include "type/pointer.h"
#include "type/type.h"

namespace ast {
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

VerifyResult Switch::VerifyType(Context *ctx) {
  std::unordered_set<const type::Type *> types;
  bool is_const = true;
  for (auto & [ expr, cond ] : cases_) {
    auto cond_result = cond->VerifyType(ctx);
    auto expr_result = expr->VerifyType(ctx);
    is_const &= cond_result.const_ && expr_result.const_;
    if (cond_result.type_ != type::Bool) { NOT_YET("handle type error"); }
    // TODO if there's an error, an unorderded_set is not helpful for giving
    // good error messages.
    types.insert(expr_result.type_);
  }
  if (types.empty()) { NOT_YET("handle type error"); }
  auto some_type = *types.begin();
  if (std::all_of(types.begin(), types.end(),
                  [&](type::Type const *t) { return t == some_type; })) {
    // TODO this might be a constant.
    return VerifyResult(ctx->set_type(this, some_type), is_const);
  } else {
    NOT_YET("handle type error");
    return VerifyResult::Error();
  }
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

std::vector<ir::Val> ast::Switch::EmitIR(Context *ctx) {
  std::unordered_map<ir::BlockIndex, ir::Val> phi_args;
  auto land_block = ir::Func::Current->AddBlock();

  // TODO handle a default value. for now, we're just not checking the very last
  // condition. This is very wrong.
  for (size_t i = 0; i < cases_.size() - 1; ++i) {
    auto & [ expr, cond ] = cases_[i];
    auto expr_block       = ir::Func::Current->AddBlock();
    auto next_block =
        ir::EarlyExitOn<true>(expr_block, cond->EmitIR(ctx)[0].reg_or<bool>());

    ir::BasicBlock::Current           = expr_block;
    auto val                          = expr->EmitIR(ctx)[0];
    phi_args[ir::BasicBlock::Current] = std::move(val);
    ir::UncondJump(land_block);

    ir::BasicBlock::Current = next_block;
  }

  phi_args[ir::BasicBlock::Current] = cases_.back().first->EmitIR(ctx)[0];
  ir::UncondJump(land_block);

  ir::BasicBlock::Current = land_block;
  auto *t                 = ctx->type_of(this);
  return {ir::MakePhi(ir::Phi(t->is_big() ? type::Ptr(t) : t), phi_args)};
}

std::vector<ir::RegisterOr<ir::Addr>> ast::Switch::EmitLVal(Context *ctx) {
  UNREACHABLE(*this);
}

}  // namespace ast
