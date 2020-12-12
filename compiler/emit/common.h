#ifndef ICARUS_IR_EMIT_COMMON_H
#define ICARUS_IR_EMIT_COMMON_H

#include <concepts>
#include <optional>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope/exec.h"
#include "ast/scope/fn.h"
#include "base/ptr_span.h"
#include "compiler/compiler.h"
#include "ir/compiled_fn.h"
#include "type/struct.h"

namespace compiler {

// Returns A function which can be executed to complete the incomplete struct
// type pointed to by `s`.
std::optional<ir::CompiledFn> StructCompletionFn(
    Compiler &c, type::Struct *s, absl::Span<ast::Declaration const> fields);

// Makes space for every local variable in this stack frame, but does not
// initialize any such object.
void MakeAllStackAllocations(Compiler &compiler, ast::FnScope const *fn_scope);

// Emits IR for each statement in `stmts`.
void EmitIrForStatements(Compiler &compiler,
                         base::PtrSpan<ast::Node const> stmts);

// Inserts all destructor calls in this scope.
void MakeAllDestructions(Compiler &compiler, ast::ExecScope const *exec_scope);

template <std::derived_from<ast::Node> NodeType, auto CompletionFn>
ir::NativeFn MakeConcreteFromGeneric(
    Compiler &compiler, NodeType const *node,
    core::Arguments<type::Typed<ir::Value>> const &args) {
  ASSERT(node->is_generic() == true);

  // Note: Cannot use structured bindings because the bindings need to be
  // captured in the lambda.
  auto find_subcontext_result = compiler.FindInstantiation(node, args);
  auto const *fn_type         = find_subcontext_result.fn_type;
  auto &context               = find_subcontext_result.context;

  auto [f, inserted] = context.add_func(node);
  if (inserted) {
    f->work_item =
        compiler.state()
            .deferred_work
            .emplace_back(std::make_unique<base::move_func<void()>>(
                [c = Compiler({.data                = context,
                               .diagnostic_consumer = compiler.diag(),
                               .importer            = compiler.importer()}),
                 node, t = f.type()]() mutable { CompletionFn(c, node, t); }))
            .get();
  }
  return f;
}

}  // namespace compiler

#endif  // ICARUS_IR_EMIT_COMMON_H
