#include "ast/comma_list.h"

#include "context.h"
#include "ir/cmd.h"
#include "type/tuple.h"

namespace ast {
std::string CommaList::to_string(size_t n) const {
  std::stringstream ss;
  if (exprs_.empty()) { return "()"; }
  if (closed_) { ss << "("; }
  auto iter = exprs_.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != exprs_.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  if (closed_) { ss << ")"; }
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

  return ctx->set_type(this, type::Tup(std::move(expr_types)));
}

void CommaList::Validate(Context *ctx) {
  for (auto &expr : exprs_) { expr->Validate(ctx); }
}

void CommaList::ExtractJumps(JumpExprs *rets) const {
  for (auto &expr : exprs_) { expr->ExtractJumps(rets); }
}

base::vector<ir::Val> CommaList::EmitIR(Context *ctx) {
  if (exprs_.size() == 1) { return {exprs_[0]->EmitIR(ctx)}; }
  base::vector<ir::Val> results;
  auto *tuple_type = &ctx->type_of(this)->as<type::Tuple>();
  auto tuple_alloc = ir::Alloca(tuple_type);
  for (size_t i = 0; i < tuple_type->entries_.size(); ++i) {
    type::EmitCopyInit(tuple_type->entries_[i], tuple_type->entries_[i],
                       exprs_[i]->EmitIR(ctx)[0],
                       ir::Field(tuple_alloc, tuple_type, i), ctx);
  }
  return {ir::Val::Reg(tuple_alloc, tuple_type)};
}

base::vector<ir::Register> CommaList::EmitLVal(Context *ctx) {
  base::vector<ir::Register> results;
  results.reserve(exprs_.size());
  for (auto &expr : exprs_) { results.push_back(expr->EmitLVal(ctx)[0]); }
  return results;
}

}  // namespace ast
