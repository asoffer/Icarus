#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::Terminal const *node) {
  return node->value();
}

// TODO: Unit tests
void Compiler::EmitAssign(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  auto const *t = data().qual_type(node)->type();
  ASSERT(to.size() == 1u);
  Visit(to[0].type()->as<type::Pointer>().pointee(), *to[0],
        type::Typed{EmitValue(node), t}, EmitCopyAssignTag{});
}

// TODO: Unit tests
void Compiler::EmitCopyInit(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitAssign(node, to);
}

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Terminal const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitAssign(node, to);
}

}  // namespace compiler