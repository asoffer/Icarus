#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

// TODO: With EmitToBuffer there's no longer a reason to allocate this.
void Compiler::EmitToBuffer(ast::ArrayLiteral const *node,
                            ir::PartialResultBuffer &out) {
  auto t     = context().qual_types(node)[0].type();
  auto alloc = state().TmpAlloca(t);
  auto typed_alloc =
      type::Typed<ir::RegOr<ir::addr_t>>(ir::RegOr<ir::addr_t>(alloc), t);
  EmitMoveInit(node, absl::MakeConstSpan(&typed_alloc, 1));
  out.append(alloc);
}

void Compiler::EmitCopyInit(
    ast::ArrayLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  type::Array const &array_type =
      context().qual_types(node)[0].type().as<type::Array>();
  auto const *data_type_ptr = type::Ptr(array_type.data_type());
  ir::Reg elem              = to[0]->reg();

  // Skip the last entry so we don't increment past the end of the array.
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    type::Typed<ir::RegOr<ir::addr_t>> to_var(elem, array_type.data_type());
    EmitCopyInit(node->elems()[i], absl::MakeConstSpan(&to_var, 1));
    elem = current_block()->Append(
        ir::PtrIncrInstruction{.addr   = elem,
                               .index  = 1,
                               .ptr    = data_type_ptr,
                               .result = current().group->Reserve()});
  }
  type::Typed<ir::RegOr<ir::addr_t>> to_var(elem, array_type.data_type());
  EmitCopyInit(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
}

void Compiler::EmitMoveInit(
    ast::ArrayLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  type::Type t = context().qual_types(node)[0].type();
  if (t == type::EmptyArray) { return; }
  type::Array const &array_type = t.as<type::Array>();
  auto *data_type_ptr           = type::Ptr(array_type.data_type());
  ir::Reg elem                  = to[0]->reg();
  // Skip the last entry so we don't increment past the end of the array.
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    type::Typed<ir::RegOr<ir::addr_t>> to_var(elem, array_type.data_type());
    EmitMoveInit(node->elems()[i], absl::MakeConstSpan(&to_var, 1));
    elem = current_block()->Append(
        ir::PtrIncrInstruction{.addr   = elem,
                               .index  = 1,
                               .ptr    = data_type_ptr,
                               .result = current().group->Reserve()});
  }
  type::Typed<ir::RegOr<ir::addr_t>> to_var(elem, array_type.data_type());
  EmitMoveInit(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
}

void Compiler::EmitCopyAssign(
    ast::ArrayLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  type::Array const &array_type =
      context().qual_types(node)[0].type().as<type::Array>();
  auto *data_type_ptr = type::Ptr(array_type.data_type());
  ir::Reg elem        = to[0]->reg();
  // Skip the last entry so we don't increment past the end of the array.
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    type::Typed<ir::RegOr<ir::addr_t>> to_var(elem, array_type.data_type());
    EmitCopyAssign(node->elems()[i], absl::MakeConstSpan(&to_var, 1));
    elem = current_block()->Append(
        ir::PtrIncrInstruction{.addr   = elem,
                               .index  = 1,
                               .ptr    = data_type_ptr,
                               .result = current().group->Reserve()});
  }
  type::Typed<ir::RegOr<ir::addr_t>> to_var(elem, array_type.data_type());
  EmitCopyAssign(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
}

void Compiler::EmitMoveAssign(
    ast::ArrayLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  type::Array const &array_type =
      context().qual_types(node)[0].type().as<type::Array>();
  auto *data_type_ptr = type::Ptr(array_type.data_type());
  ir::Reg elem        = to[0]->reg();

  // Skip the last entry so we don't increment past the end of the array.
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    type::Typed<ir::RegOr<ir::addr_t>> to_var(elem, array_type.data_type());
    EmitMoveAssign(node->elems()[i], absl::MakeConstSpan(&to_var, 1));
    elem = current_block()->Append(
        ir::PtrIncrInstruction{.addr   = elem,
                               .index  = 1,
                               .ptr    = data_type_ptr,
                               .result = current().group->Reserve()});
  }
  type::Typed<ir::RegOr<ir::addr_t>> to_var(elem, array_type.data_type());
  EmitMoveAssign(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
}

}  // namespace compiler
