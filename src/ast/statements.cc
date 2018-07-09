#include "ast/statements.h"

#include "ast/stages.h"
#include "ir/val.h"

namespace AST {
std::string Statements::to_string(size_t n) const {
  if (content_.empty()) { return ""; }

  std::stringstream ss;
  for (const auto &stmt : content_) {
    ss << std::string(n * 2, ' ') << stmt->to_string(n) << "\n";
  }
  return ss.str();
}

void Statements::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  for (auto &stmt : content_) { stmt->assign_scope(scope); }
}

void Statements::VerifyType(Context *ctx) {
  STAGE_CHECK(StartTypeVerificationStage, DoneTypeVerificationStage);
  for (auto &stmt : content_) {
    stmt->VerifyType(ctx);
    limit_to(stmt);
  }
}

void Statements::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &stmt : content_) { stmt->Validate(ctx); }
}

void Statements::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < content_.size(); ++i) {
    content_[i]->contextualize(
        correspondant->as<Statements>().content_[i].get(), replacements);
  }
}

void Statements::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &stmt : content_) { stmt->SaveReferences(scope, args); }
}

void Statements::ExtractReturns(base::vector<const Expression *> *rets) const {
  for (auto &stmt : content_) { stmt->ExtractReturns(rets); }
}

base::vector<IR::Val> AST::Statements::EmitIR(Context *ctx) {
  for (auto &stmt : content_) { stmt->EmitIR(ctx); }
  return {};
}

}  // namespace AST
