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
  ss << "switch ";
  if (expr_) { ss << "(" << expr_->to_string(n) << ") {\n"; }
  for (const auto &[expr, cond] : cases_) {
    ss << std::string((n + 1) * 2, ' ') << expr->to_string(n + 1) << " when "
       << cond->to_string(n + 1) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void Switch::assign_scope(Scope *scope) {
  scope_ = scope;
  if (expr_) { expr_->assign_scope(scope_); }
  for (auto &[expr, cond] : cases_) {
    expr->assign_scope(scope);
    cond->assign_scope(scope);
  }
}

void Switch::DependentDecls(base::Graph<Declaration *> *g,
                            Declaration *d) const {
  if (expr_) { expr_->DependentDecls(g, d); }
  for (auto &[expr, cond] : cases_) {
    expr->DependentDecls(g, d);
    cond->DependentDecls(g, d);
  }
}

VerifyResult Switch::VerifyType(Context *ctx) {
  bool is_const               = true;
  type::Type const *expr_type = nullptr;
  if (expr_) {
    ASSIGN_OR(return _, auto result, expr_->VerifyType(ctx));
    is_const &= result.const_;
    expr_type = result.type_;
  }

  std::unordered_set<const type::Type *> types;
  for (auto &[expr, cond] : cases_) {
    auto cond_result = cond->VerifyType(ctx);
    auto expr_result = expr->VerifyType(ctx);
    is_const &= cond_result.const_ && expr_result.const_;
    if (expr_) {
      static_cast<void>(expr_type);
      // TODO dispatch table
    } else {
      if (cond_result.type_ != type::Bool) { NOT_YET("handle type error"); }
    }
    // TODO if there's an error, an unorderded_set is not helpful for giving
    // good error messages.
    types.insert(expr_result.type_);
  }

  // TODO check to ensure that the type is either exhaustable or has a default.

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

void Switch::ExtractJumps(JumpExprs *rets) const {
  if (expr_) { expr_->ExtractJumps(rets); }
  for (auto &[expr, cond] : cases_) {
    expr->ExtractJumps(rets);
    cond->ExtractJumps(rets);
  }
}

std::vector<ir::Val> ast::Switch::EmitIR(Context *ctx) {
  std::unordered_map<ir::BlockIndex, ir::Val> phi_args;
  auto land_block = ir::Func::Current->AddBlock();

  // TODO handle a default value. for now, we're just not checking the very last
  // condition. This is very wrong.

  // TODO handle switching on tuples/multiple values?
  ir::Val expr_val;
  type::Type const *expr_type = nullptr;
  if (expr_) {
    expr_val  = expr_->EmitIR(ctx)[0];
    expr_type = ctx->type_of(expr_.get());
  }

  for (size_t i = 0; i < cases_.size() - 1; ++i) {
    auto &[expr, match_cond] = cases_[i];
    auto expr_block          = ir::Func::Current->AddBlock();

    auto match_val            = match_cond->EmitIR(ctx)[0];
    ir::RegisterOr<bool> cond = expr_
                                    ? ir::EmitEq(ctx->type_of(match_cond.get()),
                                                 match_val, expr_type, expr_val)
                                    : match_val.reg_or<bool>();

    auto next_block = ir::EarlyExitOn<true>(expr_block, cond);

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

}  // namespace ast
