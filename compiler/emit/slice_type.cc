#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/slice.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::SliceType const *node) {
  return ir::Value(current_block()->Append(type::SliceInstruction{
      .data_type = EmitValue(node->data_type()).get<ir::RegOr<type::Type>>(),
      .result    = builder().CurrentGroup()->Reserve()}));
}

void Compiler::EmitCopyAssign(
    ast::SliceType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitValue(node).get<ir::RegOr<type::Type>>(), *to[0]);
}

void Compiler::EmitMoveAssign(
    ast::SliceType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitValue(node).get<ir::RegOr<type::Type>>(), *to[0]);
}

void Compiler::EmitCopyInit(
    ast::SliceType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitValue(node).get<ir::RegOr<type::Type>>(), *to[0]);
}

void Compiler::EmitMoveInit(
    ast::SliceType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitValue(node).get<ir::RegOr<type::Type>>(), *to[0]);
}

}  // namespace compiler
