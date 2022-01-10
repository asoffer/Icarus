#include <vector>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/value/reg_or.h"
#include "type/scope.h"
#include "type/type.h"

namespace compiler {

// TODO: Remove forward declaration.
absl::Span<type::QualType const> VerifyType(CompilationDataReference data,
                                            ast::Node const *node);

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
          [node, d = this->data()](
              WorkResources const &wr, ir::ScopeContext scope_context,
              core::Arguments<type::Typed<ir::CompleteResultRef>> const
                  &args) mutable -> ir::Scope {
            Compiler compiler(&d);
            compiler.set_work_resources(wr);
            ASSIGN_OR(return ir::Scope(),  //
                             auto result,
                             Instantiate(compiler, node, scope_context, args));
            auto const &[params, rets_ref, context, instantiation_inserted] =
                result;
            CompilationData data{.context        = &context,
                                 .work_resources = wr,
                                 .resources      = compiler.resources()};
            Compiler c(&data);

            for (auto const *stmt : node->stmts()) { VerifyType(c, stmt); }

            context.set_qual_type(node,
                                  type::QualType::Constant(type::Scp(params)));
            auto [scope, inserted] = context.add_scope(node);
            if (inserted) {
              c.Enqueue({.kind    = WorkItem::Kind::EmitScopeBody,
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

  ICARUS_SCOPE(SetCurrent(*ir_scope, builder())) {
    builder().CurrentBlock() = ir_scope->entry();

    // TODO arguments should be renumbered to not waste space on const values
    size_t i = 0;
    for (auto const &param : node->params()) {
      absl::Span<ast::Declaration::Id const> ids = param.value->ids();
      ASSERT(ids.size() == 1u);
      state().set_addr(&ids[0], ir::Reg::Arg(i++));
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
