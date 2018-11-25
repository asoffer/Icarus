#include "ast/comma_list.h"

#include "context.h"
#include "type/tuple.h"

namespace ast {
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
    auto *expr_type = expr->VerifyType(ctx);
    if (expr_type == nullptr) { return nullptr; }
    expr_types.push_back(expr_type);
  }

  if (expr_types.empty()) {
    // TODO This is a hack and perhaps not always accurate?
    ctx->set_type(this, type::Type_);
    return type::Type_;
  } else {
    auto *tup = type::Tup(std::move(expr_types));
    ctx->set_type(this, tup);
    return tup;
  }
}

void CommaList::Validate(Context *ctx) {
  for (auto &expr : exprs) { expr->Validate(ctx); }
}

void CommaList::ExtractJumps(JumpExprs *rets) const {
  for (auto &expr : exprs) { expr->ExtractJumps(rets); }
}

base::vector<ir::Val> CommaList::EmitIR(Context *ctx) {
  base::vector<ir::Val> results;
  results.reserve(exprs.size());
  for (auto &expr : exprs) { results.push_back(expr->EmitIR(ctx)[0]); }
  return results;
}

base::vector<ir::Register> CommaList::EmitLVal(Context *ctx) {
  base::vector<ir::Register> results;
  results.reserve(exprs.size());
  for (auto &expr : exprs) { results.push_back(expr->EmitLVal(ctx)[0]); }
  return results;
}

}  // namespace ast
