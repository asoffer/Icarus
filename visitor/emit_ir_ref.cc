#include "visitor/emit_ir.h"

#include "ast/ast.h"

#include "backend/eval.h"
#include "ir/addr.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/components.h"
#include "ir/register.h"
#include "ir/str.h"
#include "misc/context.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace visitor {
using ::matcher::InheritsFrom;

std::vector<ir::RegOr<ir::Addr>> EmitIr::Ref(ast::Access const *node,
                                                  Context *ctx) {
  auto reg = node->operand()->EmitLVal(this, ctx)[0];
  auto *t  = ctx->type_of(node->operand());

  while (auto *tp = t->if_as<type::Pointer>()) {
    t   = tp->pointee;
    reg = ir::Load<ir::Addr>(reg);
  }

  ASSERT(t, InheritsFrom<type::Struct>());
  auto *struct_type = &t->as<type::Struct>();
  return {ir::Field(reg, struct_type, struct_type->index(node->member_name()))
              .get()};
}

std::vector<ir::RegOr<ir::Addr>> EmitIr::Ref(ast::CommaList const *node,
                                                  Context *ctx) {
  std::vector<ir::RegOr<ir::Addr>> results;
  results.reserve(node->exprs_.size());
  for (auto &expr : node->exprs_) {
    results.push_back(expr->EmitLVal(this, ctx)[0]);
  }
  return results;
}

std::vector<ir::RegOr<ir::Addr>> EmitIr::Ref(ast::Identifier const *node,
                                                  Context *ctx) {
  ASSERT(node->decl() != nullptr);
  return {ctx->addr(node->decl())};
}

std::vector<ir::RegOr<ir::Addr>> EmitIr::Ref(ast::Index const *node,
                                                  Context *ctx) {
  auto *lhs_type = ctx->type_of(node->lhs());
  auto *rhs_type = ctx->type_of(node->rhs());

  if (lhs_type->is<type::Array>()) {
    auto index = ir::CastTo<int64_t>(rhs_type, node->rhs()->EmitIr(this, ctx));

    auto lval = node->lhs()->EmitLVal(this, ctx)[0];
    if (!lval.is_reg_) { NOT_YET(this, ctx->type_of(node)); }
    return {ir::Index(type::Ptr(ctx->type_of(node->lhs())), lval.reg_, index)};
  } else if (auto *buf_ptr_type = lhs_type->if_as<type::BufferPointer>()) {
    auto index = ir::CastTo<int64_t>(rhs_type, node->rhs()->EmitIr(this, ctx));

    return {ir::PtrIncr(node->lhs()->EmitIr(this, ctx).get<ir::Reg>(0), index,
                        type::Ptr(buf_ptr_type->pointee))};
  } else if (lhs_type == type::ByteView) {
    // TODO interim until you remove string_view and replace it with Addr
    // entirely.
    auto index = ir::CastTo<int64_t>(rhs_type, node->rhs()->EmitIr(this, ctx));
    return {ir::PtrIncr(
        ir::GetString(
            node->lhs()->EmitIr(this, ctx).get<std::string_view>(0).val_),
        index, type::Ptr(type::Nat8))};
  } else if (auto *tup = lhs_type->if_as<type::Tuple>()) {
    auto index = ir::CastTo<int64_t>(
                     rhs_type,
                     backend::Evaluate(type::Typed{node->rhs(), rhs_type}, ctx))
                     .val_;
    return {ir::Field(node->lhs()->EmitLVal(this, ctx)[0], tup, index).get()};
  }
  UNREACHABLE(*this);
}

std::vector<ir::RegOr<ir::Addr>> EmitIr::Ref(ast::Unop const *node,
                                                  Context *ctx) {
  ASSERT(node->op == frontend::Operator::At);
  return {node->operand->EmitIr(this, ctx).get<ir::Reg>(0)};
}

}  // namespace visitor
