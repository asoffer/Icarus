#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::Index const *node, base::untyped_buffer &out) {
  type::QualType qt = context().qual_types(node->lhs())[0];
  if (qt.quals() >= type::Quals::Ref()) {
    out.append(ir::RegOr<ir::addr_t>(
        builder().PtrFix(EmitRef(node), context().qual_types(node)[0].type())));
  } else if (auto const *s = qt.type().if_as<type::Slice>()) {
    auto data = builder().Load<ir::addr_t>(
        current_block()->Append(type::SliceDataInstruction{
            .slice  = EmitValue(node->lhs()).get<ir::RegOr<ir::addr_t>>(),
            .result = builder().CurrentGroup()->Reserve(),
        }),
        type::BufPtr(s->data_type()));

    auto index = builder().CastTo<int64_t>(type::Typed<ir::Value>(
        EmitValue(node->rhs()), context().qual_types(node->rhs())[0].type()));
    out.append(ir::RegOr<ir::addr_t>(builder().PtrFix(
        builder().Index(type::Ptr(s), data, index), s->data_type())));
  } else if (auto const *array_type = qt.type().if_as<type::Array>()) {
    auto index = builder().CastTo<int64_t>(type::Typed<ir::Value>(
        EmitValue(node->rhs()), context().qual_types(node->rhs())[0].type()));
    out.append(ir::RegOr<ir::addr_t>(builder().PtrFix(
        builder().Index(type::Ptr(context().qual_types(node->lhs())[0].type()),
                        EmitValue(node->lhs()).get<ir::Reg>(), index),
        array_type->data_type())));
  } else if (auto const *buf_ptr_type =
                 qt.type().if_as<type::BufferPointer>()) {
    auto index = builder().CastTo<int64_t>(type::Typed<ir::Value>(
        EmitValue(node->rhs()), context().qual_types(node->rhs())[0].type()));

    out.append(ir::RegOr<ir::addr_t>(builder().PtrFix(
        builder().PtrIncr(EmitValue(node->lhs()).get<ir::Reg>(), index,
                          buf_ptr_type),
        buf_ptr_type->pointee())));
  } else {
    UNREACHABLE(*this, *qt);
  }
}

ir::Reg Compiler::EmitRef(ast::Index const *node) {
  type::Type lhs_type = context().qual_types(node->lhs())[0].type();
  type::Type rhs_type = context().qual_types(node->rhs())[0].type();

  if (lhs_type.is<type::Array>()) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), rhs_type));

    auto lval = EmitRef(node->lhs());
    return builder().Index(
        type::Ptr(context().qual_types(node->lhs())[0].type()), lval, index);
  } else if (auto *buf_ptr_type = lhs_type.if_as<type::BufferPointer>()) {
    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), rhs_type));

    return builder().PtrIncr(EmitValue(node->lhs()).get<ir::Reg>(), index,
                             type::Ptr(buf_ptr_type->pointee()));
  } else if (auto const *s = lhs_type.if_as<type::Slice>()) {
    auto data = builder().Load<ir::addr_t>(
        current_block()->Append(type::SliceDataInstruction{
            .slice  = EmitValue(node->lhs()).get<ir::RegOr<ir::addr_t>>(),
            .result = builder().CurrentGroup()->Reserve(),
        }),
        type::BufPtr(s->data_type()));

    auto index = builder().CastTo<int64_t>(
        type::Typed<ir::Value>(EmitValue(node->rhs()), rhs_type));

    return builder().PtrIncr(data, index, type::BufPtr(s->data_type()));
  }
  UNREACHABLE(*this, lhs_type.to_string());
}

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitMoveAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitCopyInit(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitCopyAssign(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitMoveAssign(
    ast::Index const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitMoveAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

}  // namespace compiler
