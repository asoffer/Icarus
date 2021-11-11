#ifndef ICARUS_IR_EMIT_COMMON_H
#define ICARUS_IR_EMIT_COMMON_H

#include <concepts>
#include <optional>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope.h"
#include "base/ptr_span.h"
#include "compiler/compiler.h"
#include "compiler/instantiate.h"
#include "ir/compiled_fn.h"
#include "ir/value/result_buffer.h"
#include "type/struct.h"

namespace compiler {

// Returns A function which can be executed to complete the data for an
// incomplete struct type pointed to by `s`.
std::optional<ir::CompiledFn> StructDataCompletionFn(
    Compiler &c, type::Struct *s, absl::Span<ast::Declaration const> fields);

// Returns A function which can be executed to complete the data-complete struct
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
void MakeAllDestructions(Compiler &compiler, ast::Scope const *scope);

void AppendToPartialResultBuffer(Compiler &c, type::QualType qt,
                            ast::Expression const &expr,
                            ir::PartialResultBuffer &buffer);

// Note: The `CompleteResultRef`s passed in `constant_arguments` must refer to a
// buffer that outlives the call to this function.
void EmitCall(Compiler &compiler, ast::Expression const *callee,
              core::Arguments<type::Typed<ir::CompleteResultRef>> const
                  &constant_arguments,
              absl::Span<ast::Call::Argument const> arg_exprs,
              absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to);

core::Arguments<type::Typed<ir::CompleteResultRef>> EmitConstantArguments(
    Compiler &c, absl::Span<ast::Call::Argument const> args,
    ir::CompleteResultBuffer &buffer);

void EmitArguments(
    Compiler &c, core::Params<type::QualType> const &param_qts,
    core::Params<ast::Expression const *> const &defaults,
    absl::Span<ast::Call::Argument const> arg_exprs,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &constants,
    ir::PartialResultBuffer &buffer);

}  // namespace compiler

#endif  // ICARUS_IR_EMIT_COMMON_H
