#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::ArrayLiteral const *node) {
  auto t     = context().qual_types(node)[0].type();
  auto alloc = builder().TmpAlloca(t);
  auto typed_alloc =
      type::Typed<ir::RegOr<ir::Addr>>(ir::RegOr<ir::Addr>(alloc), t);
  EmitMoveInit(node, absl::MakeConstSpan(&typed_alloc, 1));
  return ir::Value(alloc);
}

void Compiler::EmitCopyInit(
    ast::ArrayLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  type::Array const &array_type =
      context().qual_types(node)[0].type().as<type::Array>();
  auto const *data_type_ptr = type::Ptr(array_type.data_type());
  auto elem = builder().Index(type::Ptr(&array_type), to[0]->reg(), 0);
  // Skip the last entry so we don't increment past the end of the array.
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    type::Typed<ir::RegOr<ir::Addr>> to_var(elem, array_type.data_type());
    EmitCopyInit(node->elems()[i], absl::MakeConstSpan(&to_var, 1));
    elem = builder().PtrIncr(elem, 1, data_type_ptr);
  }
  type::Typed<ir::RegOr<ir::Addr>> to_var(elem, array_type.data_type());
  EmitCopyInit(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
}

void Compiler::EmitMoveInit(
    ast::ArrayLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  type::Array const &array_type =
      context().qual_types(node)[0].type().as<type::Array>();
  auto *data_type_ptr = type::Ptr(array_type.data_type());
  auto elem = builder().Index(type::Ptr(&array_type), to[0]->reg(), 0);
  // Skip the last entry so we don't increment past the end of the array.
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    type::Typed<ir::RegOr<ir::Addr>> to_var(elem, array_type.data_type());
    EmitMoveInit(node->elems()[i], absl::MakeConstSpan(&to_var, 1));
    elem = builder().PtrIncr(elem, 1, data_type_ptr);
  }
  type::Typed<ir::RegOr<ir::Addr>> to_var(elem, array_type.data_type());
  EmitMoveInit(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
}

void Compiler::EmitCopyAssign(
    ast::ArrayLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  type::Array const &array_type =
      context().qual_types(node)[0].type().as<type::Array>();
  auto *data_type_ptr = type::Ptr(array_type.data_type());
  auto elem = builder().Index(type::Ptr(&array_type), to[0]->reg(), 0);
  // Skip the last entry so we don't increment past the end of the array.
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    type::Typed<ir::RegOr<ir::Addr>> to_var(elem, array_type.data_type());
    EmitCopyAssign(node->elems()[i], absl::MakeConstSpan(&to_var, 1));
    elem = builder().PtrIncr(elem, 1, data_type_ptr);
  }
  type::Typed<ir::RegOr<ir::Addr>> to_var(elem, array_type.data_type());
  EmitCopyAssign(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
}

void Compiler::EmitMoveAssign(
    ast::ArrayLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  type::Array const &array_type =
      context().qual_types(node)[0].type().as<type::Array>();
  auto *data_type_ptr = type::Ptr(array_type.data_type());
  auto elem = builder().Index(type::Ptr(&array_type), to[0]->reg(), 0);
  // Skip the last entry so we don't increment past the end of the array.
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    type::Typed<ir::RegOr<ir::Addr>> to_var(elem, array_type.data_type());
    EmitMoveAssign(node->elems()[i], absl::MakeConstSpan(&to_var, 1));
    elem = builder().PtrIncr(elem, 1, data_type_ptr);
  }
  type::Typed<ir::RegOr<ir::Addr>> to_var(elem, array_type.data_type());
  EmitMoveAssign(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
}

}  // namespace compiler
