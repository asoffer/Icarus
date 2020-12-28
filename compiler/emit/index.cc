#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::Index const *node) {
  auto const *qt = context().qual_type(node->lhs());
  if (qt->quals() >= type::Quals::Ref()) {
    return ir::Value(builder().PtrFix(
        EmitRef(node), ASSERT_NOT_NULL(context().qual_type(node))->type()));
  }

  if (qt->type() == type::ByteView) {
    auto data = builder().ByteViewData(
        EmitValue(node->lhs()).get<ir::RegOr<ir::String>>());
    auto addr = builder().PtrIncr(
        data, EmitValue(node->rhs()).get<ir::RegOr<int64_t>>(),
        type::Ptr(type::U8));
    return builder().Load(addr, type::U8);
  } else if (auto const *array_type = qt->type().if_as<type::Array>()) {
    auto index = builder().CastTo<int64_t>(type::Typed<ir::Value>(
        EmitValue(node->rhs()), context().qual_type(node->rhs())->type()));

    return ir::Value(builder().PtrFix(
        builder().Index(type::Ptr(context().qual_type(node->lhs())->type()),
                        EmitValue(node->lhs()).get<ir::Reg>(), index),
        array_type->data_type()));
  } else {
    UNREACHABLE(*this, *qt);
  }
}

ir::Reg Compiler::EmitRef(ast::Index const *node) {
  type::Type lhs_type = context().qual_type(node->lhs())->type();
  type::Type rhs_type = context().qual_type(node->rhs())->type();

  if (lhs_type.is<type::Array>()) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), rhs_type));

    auto lval = EmitRef(node->lhs());
    return builder().Index(type::Ptr(context().qual_type(node->lhs())->type()),
                           lval, index);
  } else if (auto *buf_ptr_type = lhs_type.if_as<type::BufferPointer>()) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), rhs_type));

    return builder().PtrIncr(EmitValue(node->lhs()).get<ir::Reg>(), index,
                             type::Ptr(buf_ptr_type->pointee()));
  } else if (lhs_type == type::ByteView) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), rhs_type));
    auto str = EmitValue(node->lhs()).get<ir::RegOr<ir::String>>();
    if (str.is_reg()) {
      return builder().PtrIncr(str.reg(), index, type::Ptr(type::U8));
    } else {
      return builder().PtrIncr(str.value().addr(), index,
                               type::Ptr(type::U8));
    }
  } else if (auto *tup = lhs_type.if_as<type::Tuple>()) {
    auto maybe_val = EvaluateOrDiagnose(
        type::Typed<ast::Expression const *>(node->rhs(), rhs_type));
    if (maybe_val.empty()) { NOT_YET(); }
    auto index =
        builder()
            .CastTo<int64_t>(type::Typed<ir::Value>(maybe_val, rhs_type))
            .value();
    return builder().FieldRef(EmitRef(node->lhs()), tup, index).get();
  }
  UNREACHABLE(*this, lhs_type.to_string());
}

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_type(node)->type();
  EmitMoveAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitCopyInit(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_type(node)->type();
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitCopyAssign(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_type(node)->type();
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitMoveAssign(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_type(node)->type();
  EmitMoveAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

}  // namespace compiler
