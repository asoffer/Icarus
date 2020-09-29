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
  EmitCopyAssign(type::Typed<ir::RegOr<ir::Addr>>(
                     *to[0], to[0].type()->as<type::Pointer>().pointee()),
                 type::Typed<ir::Value>(EmitValue(node), t));
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
