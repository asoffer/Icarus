#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::BlockLiteral const *node,
                            ir::PartialResultBuffer &out) {
  LOG("BlockLiteral", "Emitting value for %p: %s", node, node->DebugString());
  // TODO: The guarantee that body verification has already happened should be
  // handled by the work queue.
  if (context().ShouldVerifyBody(node)) { VerifyBody(node); }

  std::vector<ir::RegOr<ir::Fn>> befores;
  std::vector<ir::RegOr<ir::Jump>> afters;
  befores.reserve(node->before().size());
  for (auto const &decl : node->before()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    befores.push_back(EmitAs<ir::Fn>(decl));
  }

  for (auto const &decl : node->after()) {
    ASSERT((decl->flags() & ast::Declaration::f_IsConst) != 0);
    afters.push_back(EmitAs<ir::Jump>(decl));
  }

  ir::Block b = context().add_block();
  builder().MakeBlock(b, std::move(befores), std::move(afters));
  out.append(b);
}

}  // namespace compiler
