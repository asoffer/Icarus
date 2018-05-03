#include "ast/comma_list.h"

#include "ast/verify_macros.h"
#include "type/tuple.h"

namespace AST {
std::string CommaList::to_string(size_t n) const {
  std::stringstream ss;
  if (exprs.empty()) { return "()"; }
  auto iter = exprs.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != exprs.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  return ss.str();
}

void CommaList::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  for (auto &expr : exprs) { expr->assign_scope(scope); }
}

void CommaList::ClearIdDecls() {
  stage_range_ = StageRange{};
  for (auto &expr : exprs) { expr->ClearIdDecls(); }
}

void CommaList::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  // TODO actually compute value category
  lvalue = Assign::LVal;
  for (auto &expr : exprs) {
    expr->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(expr);
    if (expr->type == type::Err) { type = type::Err; }
  }
  if (type == type::Err) {
    limit_to(StageRange::Nothing());
    return;
  } else {
    std::vector<const type::Type *> entries;
    entries.reserve(exprs.size());
    for (const auto &expr : exprs) { entries.push_back(expr->type); }
    type = type::Tup(std::move(entries));
  }
}

void CommaList::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &expr : exprs) { expr->Validate(ctx); }
}

void CommaList::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &expr : exprs) { expr->SaveReferences(scope, args); }
}
void CommaList::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < exprs.size(); ++i) {
    exprs[i]->contextualize(correspondant->as<CommaList>().exprs[i].get(),
                            replacements);
  }
}

void CommaList::ExtractReturns(std::vector<const Expression *> *rets) const {
  for (auto &expr : exprs) { expr->ExtractReturns(rets); }
}

CommaList *CommaList::Clone() const {
  auto *result = new CommaList;
  result->span = span;
  result->exprs.reserve(exprs.size());
  for (const auto &expr : exprs) { result->exprs.emplace_back(expr->Clone()); }
  return result;
}

IR::Val CommaList::EmitIR(Context *) { UNREACHABLE(this); }
IR::Val CommaList::EmitLVal(Context *) { NOT_YET(); }

}  // namespace AST
