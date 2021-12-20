#include <vector>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/value/reg_or.h"
#include "type/type.h"

namespace compiler {
// TODO: Leaking these would be totally fine. We just need to silence ASAN's
// leak checker. Longer term, we're going to rewrite these so instead of using
// type erasure they're an honest-to-goodness function in IR which will be
// appropriately owned by the module.
static std::forward_list<
    base::any_invocable<ir::Scope(WorkResources const &,ir::ScopeContext const &)>>
    invocables;

void Compiler::EmitToBuffer(ast::ScopeLiteral const *node,
                            ir::PartialResultBuffer &out) {
  invocables.push_front(
      base::any_invocable<ir::Scope(WorkResources const &,
                                    ir::ScopeContext const &)>(
          [instantiation_compiler = Compiler(&context(), resources()), node](
              WorkResources const &wr,
              ir::ScopeContext scope_context) mutable -> ir::Scope {
            ASSIGN_OR(return ir::Scope(),  //
                             auto result,
                             Instantiate(instantiation_compiler, node,
                                         scope_context));
            auto const &[params, rets_ref, context, inserted] = result;
            PersistentResources resources = instantiation_compiler.resources();
            auto compiler =
                instantiation_compiler.MakeChild(&context, resources);
            compiler.set_work_resources(wr);
            for (auto const *stmt : node->stmts()) {
              compiler.VerifyType(stmt);
            }
            // TODO: Return a real value.
            return ir::Scope();
          }));
  out.append(ir::UnboundScope(&invocables.front()));
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
