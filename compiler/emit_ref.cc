#include "compiler/compiler.h"

#include "ast/ast.h"

#include "interpretter/evaluate.h"
#include "ir/value/addr.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

static type::Typed<ir::Value> ResultsToValue(
    type::Typed<ir::Results> const &results) {
  ir::Value val(false);
  type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                   uint32_t, uint64_t, float, double, type::Type const *,
                   ir::EnumVal, ir::FlagsVal, ir::Addr, ir::String, ir::Fn>(
      results.type(), [&](auto tag) -> void {
        using T = typename decltype(tag)::type;
        val     = ir::Value(results->template get<T>(0));
      });
  return type::Typed<ir::Value>(val, results.type());
}

using ::matcher::InheritsFrom;

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Access const *node) {
  auto reg = EmitRef(node->operand());
  auto *t  = type_of(node->operand());

  while (auto *tp = t->if_as<type::Pointer>()) {
    t   = tp->pointee();
    reg = builder().Load<ir::Addr>(reg);
  }

  ASSERT(t, InheritsFrom<type::Struct>());
  auto *struct_type = &t->as<type::Struct>();
  return builder()
      .Field(reg, struct_type, struct_type->index(node->member_name()))
      .get();
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Identifier const *node) {
  ASSERT(node->decl() != nullptr) << node->DebugString();
  return data().addr(node->decl());
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Index const *node) {
  auto *lhs_type = type_of(node->lhs());
  auto *rhs_type = type_of(node->rhs());

  if (lhs_type->is<type::Array>()) {
    auto index = builder().CastTo<int64_t>(ResultsToValue(
        type::Typed<ir::Results>(EmitValue(node->rhs()), rhs_type)));

    auto lval = EmitRef(node->lhs());
    if (not lval.is_reg()) { NOT_YET(this, type_of(node)); }
    return builder().Index(type::Ptr(type_of(node->lhs())), lval.reg(), index);
  } else if (auto *buf_ptr_type = lhs_type->if_as<type::BufferPointer>()) {
    auto index = builder().CastTo<int64_t>(ResultsToValue(
        type::Typed<ir::Results>(EmitValue(node->rhs()), rhs_type)));

    return builder().PtrIncr(EmitValue(node->lhs()).get<ir::Reg>(0), index,
                             type::Ptr(buf_ptr_type->pointee()));
  } else if (lhs_type == type::ByteView) {
    // TODO interim until you remove string_view and replace it with Addr
    // entirely.
    auto index = builder().CastTo<int64_t>(ResultsToValue(
        type::Typed<ir::Results>(EmitValue(node->rhs()), rhs_type)));
    auto str   = EmitValue(node->lhs()).get<ir::String>(0);
    if (str.is_reg()) {
      return builder().PtrIncr(str.reg(), index, type::Ptr(type::Nat8));
    } else {
      return builder().PtrIncr(str.value().addr(), index,
                               type::Ptr(type::Nat8));
    }
  } else if (auto *tup = lhs_type->if_as<type::Tuple>()) {
    auto index =
        builder()
            .CastTo<int64_t>(type::Typed(
                interpretter::Evaluate(MakeThunk(node->rhs(), rhs_type)),
                rhs_type))
            .value();
    return builder().Field(EmitRef(node->lhs()), tup, index).get();
  }
  UNREACHABLE(*this);
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Unop const *node) {
  ASSERT(node->op() == frontend::Operator::At);
  return EmitValue(node->operand()).get<ir::Reg>(0);
}

}  // namespace compiler
