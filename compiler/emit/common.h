#ifndef ICARUS_IR_EMIT_COMMON_H
#define ICARUS_IR_EMIT_COMMON_H

#include <concepts>
#include <optional>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope.h"
#include "base/ptr_span.h"
#include "compiler/compiler.h"
#include "ir/compiled_fn.h"
#include "ir/value/result_buffer.h"
#include "type/struct.h"

namespace compiler {

// Structure storing all information about callables, including the values of
// any constants.
struct CallableArgumentData {
  ir::CompleteResultBuffer constants;
  core::Arguments<type::Typed<ir::CompleteResultRef>> arguments;
};

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
void MakeAllDestructions(Compiler &compiler, ast::Scope const *scope);

// Given an argument (which may be present in `constant` if it is known at
// compile-time, or simply the AST `expr`, and the qualified type of the
// parameter that this argument is being bound to, evaluate the appropriate
// binding. This involves potentially evaluating `expr` if it is not already a
// constant and applying some implicit conversions. If the expression is already
// a reference and is being bound to a pointer to its own type, we emit the
// reference rather than loading the corresponding value. Moreover non-referenc
// types get placed in temporary stack allocations in this case.
void PrepareArgument(Compiler &c, ir::PartialResultRef constant,
                     ast::Expression const *expr, type::QualType param_qt,
                     ir::PartialResultBuffer &out);
void PrepareArgument(Compiler &c, ir::PartialResultRef constant,
                     type::QualType arg_qt, type::QualType param_qt,
                     ir::PartialResultBuffer &out);

void AppendToPartialResultBuffer(Compiler &c, type::QualType qt,
                            ast::Expression const &expr,
                            ir::PartialResultBuffer &buffer);

ir::PartialResultBuffer EmitConstantPartialResultBuffer(
    Compiler &c, absl::Span<ast::Call::Argument const> args);
CallableArgumentData EmitConstantArguments(
    Compiler &c, absl::Span<ast::Call::Argument const> args);

void EmitCall(Compiler &compiler, ast::Expression const *callee,
              CallableArgumentData argument_data,
              absl::Span<ast::Call::Argument const> arg_exprs,
              absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to);

}  // namespace compiler

#endif  // ICARUS_IR_EMIT_COMMON_H
