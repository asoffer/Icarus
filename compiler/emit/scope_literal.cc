#include <vector>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/value/reg_or.h"
#include "type/type.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::ScopeLiteral const *node,
                            ir::PartialResultBuffer &out) {
  // TODO: Long-term we shouldn't heap-allocate this, but each of these needs to
  // be separately allocated and long-lived anyway there's not much harm in
  // ignoring the proper ownership story for the time being.
  out.append(
      ir::UnboundScope(new base::any_invocable<std::optional<ir::Scope>(
                           ir::ScopeContext const &)>(
          [instantiation_compiler = Compiler(&context(), resources()),
           node](ir::ScopeContext const &scope_context) mutable
          -> std::optional<ir::Scope> {
            ASSIGN_OR(return std::nullopt,  //
                             auto result,
                             Instantiate(instantiation_compiler, node,
                                         scope_context));
            auto const &[params, rets_ref, context, inserted] = result;
            PersistentResources resources = instantiation_compiler.resources();
            auto compiler =
                instantiation_compiler.MakeChild(&context, resources);
            // TODO: Is this necessary?
            // compiler.set_work_resources(wr);
            for (auto const *stmt : node->stmts()) {
              compiler.VerifyType(stmt);
            }
            // TODO: Return a real value.
            return std::nullopt;
          })));
}

bool Compiler::EmitScopeBody(ast::ScopeLiteral const *node) {
  LOG("EmitScopeBody", "Scope %s", node->DebugString());
  ir::Scope ir_scope     = context().FindScope(node);

  ICARUS_SCOPE(ir::SetCurrent(*ir_scope, builder())) {
    builder().CurrentBlock() = ir_scope->entry();
    // TODO: arguments should be renumbered to not waste space on const
    // values
    int32_t i = 0;
    for (auto const &param : node->params()) {
      // TODO: Support multiple declarations?
      builder().set_addr(&param.value->ids()[0], ir::Reg::Arg(i++));
    }

    MakeAllStackAllocations(*this, &node->body_scope());
    EmitIrForStatements(*this, node->stmts());

    // TODO: Destructors are significantly more complicated than this because we
    // need to ensure we call them even on paths that exit scopes without
    // computing values.
    MakeAllDestructions(*this, &node->body_scope());
    builder().ReturnJump();
  }

  return true;
}

}  // namespace compiler
