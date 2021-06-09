#include "ast/ast.h"
#include "compiler/compiler.h"
#include "diagnostic/message.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::PatternMatch const *node) {
  ir::Value result;
  type::Type t;
  base::untyped_buffer result_buffer;
  if (node->is_binary()) {
    t             = context().qual_types(node)[0].type();
    result_buffer = EvaluateToBufferOrDiagnose(
        type::Typed<ast::Expression const *>(&node->expr(), t));
  } else {
    t = type::Type_;
    type::Type unary_result =
        context().arg_type(node->expr().as<ast::Declaration>().ids()[0].name());
    result_buffer.append(unary_result);
    result = ir::Value(unary_result);
  }

  PatternMatchingContext pmc{
      .type  = t,
      .value = result_buffer,
  };
  if (PatternMatch(&node->pattern(), pmc)) {
    for (auto &[name, buffer] : pmc.bindings) {
      auto const *id =
          module::AllVisibleDeclsTowardsRoot(node->scope(), name)[0];
      context().SetConstant(id, std::move(buffer));
    }
  } else {
    NOT_YET();
  }

  return result;
}

}  // namespace compiler

