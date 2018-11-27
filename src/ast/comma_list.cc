#include "ast/comma_list.h"

#include "context.h"
#include "type/tuple.h"

namespace ast {
std::string CommaList::to_string(size_t n) const {
  std::stringstream ss;
  if (exprs_.empty()) { return "()"; }
  auto iter = exprs_.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != exprs_.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  return ss.str();
}

void CommaList::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto &expr : exprs_) { expr->assign_scope(scope); }
}

type::Type const *CommaList::VerifyType(Context *ctx) {
  base::vector<const type::Type *> expr_types;
  expr_types.reserve(exprs_.size());
  for (auto &expr : exprs_) { expr_types.push_back(expr->VerifyType(ctx)); }
  if (std::any_of(expr_types.begin(), expr_types.end(),
                  [](type::Type const *t) { return t == nullptr; })) {
    return nullptr;
  }

  if (expr_types.empty()) {
    // TODO This is a hack and definitely not always accurate. Especially when
    // ArrayLiteral calls this code.
    return ctx->set_type(this, type::Type_);
  } else {
    return ctx->set_type(this, type::Tup(std::move(expr_types)));
  }
}

void CommaList::Validate(Context *ctx) {
  for (auto &expr : exprs_) { expr->Validate(ctx); }
}

void CommaList::ExtractJumps(JumpExprs *rets) const {
  for (auto &expr : exprs_) { expr->ExtractJumps(rets); }
}

base::vector<ir::Val> CommaList::EmitIR(Context *ctx) {
  base::vector<ir::Val> results;
  results.reserve(exprs_.size());
  for (auto &expr : exprs_) { results.push_back(expr->EmitIR(ctx)[0]); }
  return results;
}

base::vector<ir::Register> CommaList::EmitLVal(Context *ctx) {
  base::vector<ir::Register> results;
  results.reserve(exprs_.size());
  for (auto &expr : exprs_) { results.push_back(expr->EmitLVal(ctx)[0]); }
  return results;
}

}  // namespace ast
