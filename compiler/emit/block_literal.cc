#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::BlockLiteral const *node) {
  LOG("BlockLiteral", "Emitting value for %p: %s", node, node->DebugString());
  // TODO: The guarantee that body verification has already happened should be
  // handled by the work queue.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  std::vector<ir::RegOr<ir::Fn>> befores;
  std::vector<ir::RegOr<ir::Jump>> afters;
  befores.reserve(node->before().size());
  for (auto const &decl : node->before()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    befores.push_back(EmitValue(decl).get<ir::RegOr<ir::Fn>>());
  }

  for (auto const &decl : node->after()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    afters.push_back(EmitValue(decl).get<ir::RegOr<ir::Jump>>());
  }

  return ir::Value(builder().MakeBlock(context().add_block(),
                                       std::move(befores), std::move(afters)));
}

}  // namespace compiler
