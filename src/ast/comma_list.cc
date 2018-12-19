#include "ast/comma_list.h"

#include "context.h"
#include "ir/cmd.h"
#include "type/tuple.h"

namespace ast {
std::string CommaList::to_string(size_t n) const {
  std::stringstream ss;
  if (exprs_.empty()) { return "()"; }
  if (parenthesized_) { ss << "("; }
  auto iter = exprs_.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != exprs_.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  if (parenthesized_) { ss << ")"; }
  return ss.str();
}

void CommaList::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto &expr : exprs_) { expr->assign_scope(scope); }
}

type::Type const *CommaList::VerifyType(Context *ctx) {
  base::vector<const type::Type *> expr_types;
  expr_types.reserve(exprs_.size());
  for (auto &expr : exprs_) {
    auto *t = expr->VerifyType(ctx);
    if (expr->needs_expansion()) {
      auto &entries = t->as<type::Tuple>().entries_;
      expr_types.insert(expr_types.end(), entries.begin(), entries.end());
    } else {
      expr_types.push_back(t);
    }
  }
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
  base::vector<ir::Val> results;
  auto *tuple_type = &ctx->type_of(this)->as<type::Tuple>();
  // TODO this is a hack. I'm still not sure what counts as a tuple and what
  // counts as atype
  if (tuple_type->entries_.empty()) { return {ir::Val(type::Tup({}))}; }

  auto tuple_alloc = ir::Alloca(tuple_type);

  size_t index = 0;
  for (auto &expr : exprs_) {
    if (expr->needs_expansion()) {
      for (auto const &val : expr->EmitIR(ctx)) {
        type::EmitCopyInit(tuple_type->entries_[index],
                           tuple_type->entries_[index], val,
                           ir::Field(tuple_alloc, tuple_type, index), ctx);
        ++index;
      }
    } else {
      type::EmitCopyInit(tuple_type->entries_[index],
                         tuple_type->entries_[index], expr->EmitIR(ctx)[0],
                         ir::Field(tuple_alloc, tuple_type, index), ctx);
      ++index;
    }
  }
  return {ir::Val::Reg(tuple_alloc, tuple_type)};
}

base::vector<ir::RegisterOr<ir::Addr>> CommaList::EmitLVal(Context *ctx) {
  base::vector<ir::RegisterOr<ir::Addr>> results;
  results.reserve(exprs_.size());
  for (auto &expr : exprs_) { results.push_back(expr->EmitLVal(ctx)[0]); }
  return results;
}

}  // namespace ast
