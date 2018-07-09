#include "ast/switch.h"

#include <numeric>
#include <sstream>
#include <unordered_set>
#include "ast/verify_macros.h"
#include "base/util.h"
#include "type/type.h"
#include "ir/func.h"

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
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  for (auto & [ expr, cond ] : cases_) {
    expr->assign_scope(scope);
    cond->assign_scope(scope);
  }
}

void Switch::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::RVal;
  std::unordered_set<const type::Type *> types;
  for (auto & [ expr, cond ] : cases_) {
    cond->VerifyType(ctx);
    expr->VerifyType(ctx);
    if (cond->type != type::Bool) { NOT_YET("handle type error"); }
    // TODO if there's an error, an unorderded_set is not helpful for giving
    // good error messages.
    types.insert(expr->type);
  }
  if (types.empty()) { NOT_YET("handle type error"); }
  type =
      std::accumulate(types.begin(), types.end(), *types.begin(), type::Join);
  if (type == nullptr) {
    type = type::Err;
    NOT_YET("handle type error");
  }
}

void Switch::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);

  for (auto & [ expr, cond ] : cases_) {
    expr->Validate(ctx);
    cond->Validate(ctx);
  }
}

void Switch::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto & [ expr, cond ] : cases_) {
    expr->SaveReferences(scope, args);
    cond->SaveReferences(scope, args);
  }
}

void Switch::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < cases_.size(); ++i) {
    cases_[i].first->contextualize(
        correspondant->as<Switch>().cases_[i].first.get(), replacements);
    cases_[i].second->contextualize(
        correspondant->as<Switch>().cases_[i].second.get(), replacements);
  }
}

void Switch::ExtractReturns(base::vector<const Expression *> * rets) const {
  for (auto & [ expr, cond ] : cases_) {
    expr->ExtractReturns(rets);
    cond->ExtractReturns(rets);
  }
}

Switch *Switch::Clone() const {
  auto *result = new Switch;
  result->span = span;
  result->cases_.reserve(cases_.size());
  for (const auto & [ expr, cond ] : cases_) {
    result->cases_.emplace_back(base::wrap_unique(expr->Clone()),
                                base::wrap_unique(cond->Clone()));
  }

  return result;
}

base::vector<IR::Val> AST::Switch::EmitIR(Context *ctx) {
  base::vector<IR::Val> phi_args;
  phi_args.reserve(2 * cases_.size());
  auto land_block = IR::Func::Current->AddBlock();

  // TODO handle a default value. for now, we're just not checking the very last
  // condition. This is very wrong.
  for (size_t i= 0; i < cases_.size() - 1; ++i) {
    auto & [ expr, cond ] = cases_[i];
    auto expr_block = IR::Func::Current->AddBlock();
    auto next_block = IR::EarlyExitOn<true>(expr_block, cond->EmitIR(ctx)[0]);

    IR::BasicBlock::Current = expr_block;
    auto val                = expr->EmitIR(ctx)[0];
    phi_args.push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
    phi_args.push_back(std::move(val));
    IR::UncondJump(land_block);

    IR::BasicBlock::Current = next_block;
  }

  auto val = cases_.back().first->EmitIR(ctx)[0];
  IR::UncondJump(land_block);

  phi_args.push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
  phi_args.push_back(std::move(val));
 
  IR::BasicBlock::Current = land_block;
  auto phi           = IR::Phi(type);
  IR::Func::Current->SetArgs(phi, std::move(phi_args));

  return {IR::Func::Current->Command(phi).reg()};
}

base::vector<IR::Val> AST::Switch::EmitLVal(Context *ctx) { UNREACHABLE(*this); }

}  // namespace AST
