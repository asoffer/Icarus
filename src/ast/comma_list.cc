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
  scope_ = scope;
  for (auto &expr : exprs) { expr->assign_scope(scope); }
}

type::Type const *CommaList::VerifyType(Context *ctx) {
  base::vector<const type::Type *> expr_types;
  expr_types.reserve(exprs.size());
  for (auto &expr : exprs) {
    LOG << "---";
    auto *expr_type = expr->VerifyType(ctx);
    LOG << this << ": " << expr_type << "(" << (uintptr_t)expr.get() << ")";
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(expr);
    if (expr_type == nullptr) { return nullptr; }
    expr_types.push_back(expr_type);
  }

  if (expr_types.empty()) {
    // TODO This is a hack and perhaps not always accurate?
    ctx->mod_->set_type(ctx->bound_constants_, this, type::Type_);
    return type::Type_;
  } else {
    auto *tup = type::Tup(std::move(expr_types));
    ctx->mod_->set_type(ctx->bound_constants_, this, tup);
    return tup;
  }
}

void CommaList::Validate(Context *ctx) {
  for (auto &expr : exprs) { expr->Validate(ctx); }
}

void CommaList::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &expr : exprs) { expr->SaveReferences(scope, args); }
}
void CommaList::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < exprs.size(); ++i) {
    exprs[i]->contextualize(correspondant->as<CommaList>().exprs[i].get(),
                            replacements);
  }
}

void CommaList::ExtractReturns(base::vector<const Expression *> *rets) const {
  for (auto &expr : exprs) { expr->ExtractReturns(rets); }
}

CommaList *CommaList::Clone() const {
  auto *result = new CommaList;
  result->span = span;
  result->exprs.reserve(exprs.size());
  for (const auto &expr : exprs) { result->exprs.emplace_back(expr->Clone()); }
  return result;
}

base::vector<IR::Val> CommaList::EmitIR(Context *ctx) {
  base::vector<IR::Val> results;
  results.reserve(exprs.size());
  for (auto &expr : exprs) { results.push_back(expr->EmitIR(ctx)[0]); }
  return results;
}

base::vector<IR::Register> CommaList::EmitLVal(Context *ctx) {
  base::vector<IR::Register> results;
  results.reserve(exprs.size());
  for (auto &expr : exprs) { results.push_back(expr->EmitLVal(ctx)[0]); }
  return results;
}

}  // namespace AST
