#include "ast/comma_list.h"

#include "ir/cmd.h"
#include "misc/context.h"
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

void CommaList::DependentDecls(DeclDepGraph *g,
                               Declaration *d) const {
  for (auto const &expr : exprs_) { expr->DependentDecls(g, d); }
}

ir::Results CommaList::EmitIr(Context *ctx) {
  auto *tuple_type = &ctx->type_of(this)->as<type::Tuple>();
  // TODO this is a hack. I'm still not sure what counts as a tuple and what
  // counts as atype
  if (tuple_type->entries_.empty()) { return ir::Results{type::Tup({})}; }

  auto tuple_alloc = ir::TmpAlloca(tuple_type, ctx);

  size_t index = 0;
  for (auto &expr : exprs_) {
    if (expr->needs_expansion()) {
      auto results = expr->EmitIr(ctx);
      for (size_t i = 0; i < results.size(); ++i) {
        type::EmitCopyInit(tuple_type->entries_[index], results.GetResult(i),
                           ir::Field(tuple_alloc, tuple_type, index), ctx);
        ++index;
      }
    } else {
      type::EmitCopyInit(tuple_type->entries_[index], expr->EmitIr(ctx),
                         ir::Field(tuple_alloc, tuple_type, index), ctx);
      ++index;
    }
  }
  return ir::Results{tuple_alloc};
}

std::vector<ir::RegisterOr<ir::Addr>> CommaList::EmitLVal(Context *ctx) {
  std::vector<ir::RegisterOr<ir::Addr>> results;
  results.reserve(exprs_.size());
  for (auto &expr : exprs_) { results.push_back(expr->EmitLVal(ctx)[0]); }
  return results;
}

void CommaList::EmitMoveInit(type::Typed<ir::Reg> reg, Context *ctx) {
  size_t index  = 0;
  auto const &t = reg.type()->as<type::Pointer>().pointee->as<type::Tuple>();
  for (auto &expr : exprs_) {
    if (expr->needs_expansion()) {
      auto results = expr->EmitIr(ctx);
      for (size_t i = 0; i < results.size(); ++i) {
        type::EmitMoveInit(t.entries_[index], results.GetResult(i),
                           ir::Field(reg.get(), &t, index), ctx);
        ++index;
      }
    } else {
      expr->EmitMoveInit(ir::Field(reg.get(), &t, index), ctx);
      ++index;
    }
  }
}

void CommaList::EmitCopyInit(type::Typed<ir::Reg> reg, Context *ctx) {
  size_t index  = 0;
  auto const &t = reg.type()->as<type::Pointer>().pointee->as<type::Tuple>();
  for (auto &expr : exprs_) {
    if (expr->needs_expansion()) {
      auto results = expr->EmitIr(ctx);
      for (size_t i = 0; i < results.size(); ++i) {
        type::EmitCopyInit(t.entries_[index], results.GetResult(i),
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
