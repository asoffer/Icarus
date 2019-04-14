#include "ast/switch.h"

#include <numeric>
#include <sstream>

#include "absl/container/flat_hash_set.h"
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
  for (const auto &[body, cond] : cases_) {
    ss << std::string((n + 1) * 2, ' ') << body->to_string(n + 1) << " when "
       << cond->to_string(n + 1) << "\n";
  }
  ss << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void Switch::assign_scope(core::Scope *scope) {
  scope_ = scope;
  if (expr_) { expr_->assign_scope(scope_); }
  for (auto &[body, cond] : cases_) {
    body->assign_scope(scope);
    cond->assign_scope(scope);
  }
}

void Switch::DependentDecls(DeclDepGraph *g,
                            Declaration *d) const {
  if (expr_) { expr_->DependentDecls(g, d); }
  for (auto &[body, cond] : cases_) {
    body->DependentDecls(g, d);
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

  absl::flat_hash_set<type::Type const *> types;
  bool err = false;
  for (auto &[body, cond] : cases_) {
    auto cond_result = cond->VerifyType(ctx);
    auto body_result = body->VerifyType(ctx);
    err |= !cond_result || !body_result;
    if (err) {
      base::Log() << body->to_string(0);
      NOT_YET();
      continue;
    }

    is_const &= cond_result.const_ && body_result.const_;
    if (expr_) {
      static_cast<void>(expr_type);
      // TODO dispatch table
    } else {
      if (cond_result.type_ != type::Bool) { NOT_YET("handle type error"); }
    }
    // TODO if there's an error, an unorderded_set is not helpful for giving
    // good error messages.
    if (body->is<Expression>()) {
      // TODO check that it's actually a jump
      types.insert(body_result.type_);
    }
  }
  if (err) { return VerifyResult::Error(); }

  // TODO check to ensure that the type is either exhaustable or has a default.

  if (types.empty()) {
    return ctx->set_result(this, VerifyResult(type::Void(), is_const));
  }
  auto some_type = *types.begin();
  if (std::all_of(types.begin(), types.end(),
                  [&](type::Type const *t) { return t == some_type; })) {
    // TODO this might be a constant.
    return ctx->set_result(this, VerifyResult(some_type, is_const));
  } else {
    for (auto &t:types) {
      base::Log() << (!t ? "<>" : t->to_string());
    }
    NOT_YET("handle type error");
    return VerifyResult::Error();
  }
}

void Switch::ExtractJumps(JumpExprs *rets) const {
  if (expr_) { expr_->ExtractJumps(rets); }
  for (auto &[body, cond] : cases_) {
    body->ExtractJumps(rets);
    cond->ExtractJumps(rets);
  }
}

ir::Results ast::Switch::EmitIr(Context *ctx) {
  absl::flat_hash_map<ir::BlockIndex, ir::Results> phi_args;
  auto land_block = ir::Func::Current->AddBlock();
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

  for (size_t i = 0; i < cases_.size() - 1; ++i) {
    auto &[body, match_cond] = cases_[i];
    auto expr_block          = ir::Func::Current->AddBlock();

    ir::Results match_val     = match_cond->EmitIr(ctx);
    ir::RegisterOr<bool> cond = expr_
                                    ? ir::EmitEq(ctx->type_of(match_cond.get()),
                                                 match_val, expr_type, expr_results)
                                    : match_val.get<bool>(0);

    auto next_block = ir::EarlyExitOn<true>(expr_block, cond);

    ir::BasicBlock::Current           = expr_block;
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

  phi_args[ir::BasicBlock::Current] = cases_.back().first->EmitIr(ctx);
  ir::UncondJump(land_block);

  ir::BasicBlock::Current = land_block;
  if (t == type::Void()) {
    return ir::Results{};
  } else {
    return ir::MakePhi(t, ir::Phi(t->is_big() ? type::Ptr(t) : t), phi_args);
  }
}

}  // namespace ast
