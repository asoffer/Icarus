#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::ArrayType const *node) {
  auto value = EmitValue(node->data_type()).get<ir::RegOr<type::Type>>();
  // Size must be at least 1 by construction, so `.size() - 1` will not
  // overflow.
  for (int i = node->lengths().size() - 1; i >= 0; --i) {
    value = builder().Array(
        EmitValue(node->length(i)).get<ir::RegOr<int64_t>>(), value);
  }
  return ir::Value(value);
}

void Compiler::EmitAssign(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  builder().Store(EmitValue(node).get<ir::RegOr<type::Type>>(), *to[0]);
}

void Compiler::EmitCopyInit(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitAssign(node, to);
}

void Compiler::EmitMoveInit(
    ast::ArrayType const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitAssign(node, to);
}

}  // namespace compiler
