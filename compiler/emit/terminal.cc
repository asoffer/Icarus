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
  Visit(t, *to[0], type::Typed{EmitValue(node), t}, EmitCopyAssignTag{});
}


}  // namespace compiler
