#include "ast/ast.h"
#include "compiler/compiler.h"
#include "diagnostic/message.h"


namespace compiler {

ir::Value Compiler::EmitValue(ast::PatternMatch const *node) {
  auto t             = context().qual_types(node)[0].type();
  auto result_buffer = EvaluateToBufferOrDiagnose(
      type::Typed<ast::Expression const *>(&node->expr(), t));
  PatternMatchingContext pmc{
      .node  = &node->expr(),
      .type  = t,
      .value = result_buffer,
  };
  if (PatternMatch(&node->pattern(), pmc)) {
    for (auto &[name, buffer] : pmc.bindings) {
      auto const *id =
          module::AllVisibleDeclsTowardsRoot(node->scope(), name)[0];
      context().SetConstant(id, std::move(buffer));
    }
  }

  return ir::Value();
}

}  // namespace compiler

