#include <vector>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/value/reg_or.h"
#include "type/scope.h"
#include "type/type.h"

namespace compiler {
// TODO: Leaking these would be totally fine. We just need to silence ASAN's
// leak checker. Longer term, we're going to rewrite these so instead of using
// type erasure they're an honest-to-goodness function in IR which will be
// appropriately owned by the module.
static std::forward_list<ir::UnboundScope::Data> scope_data;

void Compiler::EmitToBuffer(ast::ScopeLiteral const *node,
                            ir::PartialResultBuffer &out) {
  scope_data.push_front(ir::UnboundScope::Data{
      .literal = node,
      .f       = base::any_invocable<ir::Scope(
          WorkResources const &, ir::ScopeContext const &,
          core::Arguments<type::Typed<ir::CompleteResultRef>> const &)>(
          [instantiation_compiler = Compiler(&context(), resources()), node](
              WorkResources const &wr, ir::ScopeContext scope_context,
              core::Arguments<type::Typed<ir::CompleteResultRef>> const
                  &args) mutable -> ir::Scope {
            instantiation_compiler.set_work_resources(wr);
            ASSIGN_OR(return ir::Scope(),  //
                             auto result,
                             Instantiate(instantiation_compiler, node,
                                         scope_context, args));
            auto const &[params, rets_ref, context, instantiation_inserted] =
                result;
            PersistentResources resources = instantiation_compiler.resources();
            auto compiler =
                instantiation_compiler.MakeChild(&context, resources);
            compiler.set_work_resources(wr);
            for (auto const *stmt : node->stmts()) {
              compiler.VerifyType(stmt);
            }

            context.set_qual_type(node,
                                  type::QualType::Constant(type::Scp(params)));
            auto [scope, inserted] = context.add_scope(node);
            if (inserted) {
              ICARUS_SCOPE(ir::SetCurrent(*scope, compiler.builder())) {
                size_t num_blocks = scope_context.size();
                for (size_t i = 0; i < num_blocks; ++i) {
                  auto *entry_to_block =
                      compiler.builder().CurrentGroup()->AppendBlock();
                  auto *exit_from_block =
                      compiler.builder().CurrentGroup()->AppendBlock();
                  scope.add_connection(entry_to_block, exit_from_block);
                }
              }

              compiler.Enqueue({.kind    = WorkItem::Kind::EmitScopeBody,
                                .node    = node,
                                .context = &context});
            }

            return scope;
          })});
  out.append(ir::UnboundScope(&scope_data.front()));
}

bool Compiler::EmitScopeBody(ast::ScopeLiteral const *node) {
  LOG("EmitScopeBody", "Scope %s", node->DebugString());
  ir::Scope ir_scope = context().FindScope(node);
  state().scopes.push_back(ir_scope);
  absl::Cleanup cleanup = [&] { state().scopes.pop_back(); };

  ICARUS_SCOPE(ir::SetCurrent(*ir_scope, builder())) {
    builder().CurrentBlock() = ir_scope->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      absl::Span<ast::Declaration::Id const> ids = param.value->ids();
      ASSERT(ids.size() == 1u);
      builder().set_addr(&ids[0], ir::Reg::Arg(i++));
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
