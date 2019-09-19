#include "visitor/emit_ir.h"

#include "ast/ast.h"

#include "backend/eval.h"
#include "ir/addr.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/components.h"
#include "ir/str.h"
#include "misc/context.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace visitor {
using ::matcher::InheritsFrom;

std::vector<ir::RegOr<ir::Addr>> TraditionalCompilation::EmitRef(
    ast::Access const *node) {
  auto reg = node->operand()->EmitRef(this)[0];
  auto *t  = context().type_of(node->operand());

  while (auto *tp = t->if_as<type::Pointer>()) {
    t   = tp->pointee;
    reg = ir::Load<ir::Addr>(reg);
  }

  ASSERT(t, InheritsFrom<type::Struct>());
  auto *struct_type = &t->as<type::Struct>();
  return {ir::Field(reg, struct_type, struct_type->index(node->member_name()))
              .get()};
}

std::vector<ir::RegOr<ir::Addr>> TraditionalCompilation::EmitRef(
    ast::CommaList const *node) {
  std::vector<ir::RegOr<ir::Addr>> results;
  results.reserve(node->exprs_.size());
  for (auto &expr : node->exprs_) { results.push_back(expr->EmitRef(this)[0]); }
  return results;
}

std::vector<ir::RegOr<ir::Addr>> TraditionalCompilation::EmitRef(
    ast::Identifier const *node) {
  ASSERT(node->decl() != nullptr);
  return {context().addr(node->decl())};
}

std::vector<ir::RegOr<ir::Addr>> TraditionalCompilation::EmitRef(
    ast::Index const *node) {
  auto *lhs_type = context().type_of(node->lhs());
  auto *rhs_type = context().type_of(node->rhs());

  if (lhs_type->is<type::Array>()) {
    auto index = ir::CastTo<int64_t>(rhs_type, node->rhs()->EmitValue(this));

    auto lval = node->lhs()->EmitRef(this)[0];
    if (!lval.is_reg()) { NOT_YET(this, context().type_of(node)); }
    return {ir::Index(type::Ptr(context().type_of(node->lhs())), lval.reg(),
                      index)};
  } else if (auto *buf_ptr_type = lhs_type->if_as<type::BufferPointer>()) {
    auto index = ir::CastTo<int64_t>(rhs_type, node->rhs()->EmitValue(this));

    return {ir::PtrIncr(node->lhs()->EmitValue(this).get<ir::Reg>(0), index,
                        type::Ptr(buf_ptr_type->pointee))};
  } else if (lhs_type == type::ByteView) {
    // TODO interim until you remove string_view and replace it with Addr
    // entirely.
    auto index = ir::CastTo<int64_t>(rhs_type, node->rhs()->EmitValue(this));
    return {ir::PtrIncr(
        ir::GetString(
            node->lhs()->EmitValue(this).get<std::string_view>(0).value()),
        index, type::Ptr(type::Nat8))};
  } else if (auto *tup = lhs_type->if_as<type::Tuple>()) {
    auto index =
        ir::CastTo<int64_t>(
            rhs_type,
            backend::Evaluate(type::Typed{node->rhs(), rhs_type}, &context()))
            .value();
    return {ir::Field(node->lhs()->EmitRef(this)[0], tup, index).get()};
  }
  UNREACHABLE(*this);
}

std::vector<ir::RegOr<ir::Addr>> TraditionalCompilation::EmitRef(
    ast::Unop const *node) {
  ASSERT(node->op == frontend::Operator::At);
  return {node->operand->EmitValue(this).get<ir::Reg>(0)};
}

}  // namespace visitor
