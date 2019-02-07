#include "ast/comma_list.h"

#include "misc/context.h"
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

std::optional<std::vector<VerifyResult>> CommaList::VerifyWithoutSetting(
    Context *ctx) {
  base::vector<VerifyResult> results;
  results.reserve(exprs_.size());
  for (auto &expr : exprs_) {
    auto r = expr->VerifyType(ctx);
    if (expr->needs_expansion()) {
      auto &entries = r.type_->as<type::Tuple>().entries_;
      for (auto *t : entries) { results.emplace_back(t, r.const_); }
    } else {
      results.push_back(r);
    }
  }
  if (std::any_of(results.begin(), results.end(),
                  [](VerifyResult const &r) { return !r.ok(); })) {
    return std::nullopt;
  }
  return results;
}

VerifyResult CommaList::VerifyType(Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), auto results,
                   VerifyWithoutSetting(ctx));
  base::vector<type::Type const *> ts;
  ts.reserve(results.size());
  bool is_const = true;
  for (auto const &r : results) {
    ts.push_back(r.type_);
    is_const &= r.const_;
  }
  return VerifyResult(ctx->set_type(this, type::Tup(std::move(ts))), is_const);
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

  auto tuple_alloc = ir::TmpAlloca(tuple_type, ctx);

  size_t index = 0;
  for (auto &expr : exprs_) {
    if (expr->needs_expansion()) {
      for (auto const &val : expr->EmitIR(ctx)) {
        type::EmitCopyInit(tuple_type->entries_[index], val,
                           ir::Field(tuple_alloc, tuple_type, index), ctx);
        ++index;
      }
    } else {
      type::EmitCopyInit(tuple_type->entries_[index], expr->EmitIR(ctx)[0],
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

void CommaList::EmitMoveInit(type::Typed<ir::Register> reg, Context *ctx) {
  size_t index = 0;
  auto const &t = reg.type()->as<type::Pointer>().pointee->as<type::Tuple>();
  for (auto &expr : exprs_) {
    if (expr->needs_expansion()) {
      for (auto const &val : expr->EmitIR(ctx)) {
        // TODO handle this case.
        type::EmitMoveInit(t.entries_[index], val,
                           ir::Field(reg.get(), &t, index), ctx);
        ++index;
      }
    } else {
      expr->EmitMoveInit(ir::Field(reg.get(), &t, index), ctx);
      ++index;
    }
  }
}

void CommaList::EmitCopyInit(type::Typed<ir::Register> reg, Context *ctx) {
  size_t index = 0;
  auto const &t = reg.type()->as<type::Pointer>().pointee->as<type::Tuple>();
  for (auto &expr : exprs_) {
    if (expr->needs_expansion()) {
      for (auto const &val : expr->EmitIR(ctx)) {
        // TODO handle this case.
        type::EmitCopyInit(t.entries_[index], val,
                           ir::Field(reg.get(), &t, index), ctx);
        ++index;
      }
    } else {
      expr->EmitCopyInit(ir::Field(reg.get(), &t, index), ctx);
      ++index;
    }
  }

}

}  // namespace ast
