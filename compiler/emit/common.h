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
#include "ir/value/argument_buffer.h"
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
void MakeAllDestructions(Compiler &compiler, ast::Scope const *scope);

// Given an argument (which may be present in `constant` if it is known at
// compile-time, or simply the AST `expr`, and the qualified type of the
// parameter that this argument is being bound to, evaluate the appropriate
// binding. This involves potentially evaluating `expr` if it is not already a
// constant and applying some implicit conversions. If the expression is already
// a reference and is being bound to a pointer to its own type, we emit the
// reference rather than loading the corresponding value. Moreover non-referenc
// types get placed in temporary stack allocations in this case.
base::untyped_buffer PrepareArgument(Compiler &c, ir::Value constant,
                                     ast::Expression const *expr,
                                     type::QualType param_qt);
// Same as the above overload but rather than emitting the code for expressions
// on the fly, this overload is to be used when the value has already been
// computed, but you know the qualified type. In particular arg_value needs to
// be non-empty when arg_qt is constant. When arg_qt is a reference type,
// arg_value holds a reference to the relevant value rather than the actual
// value itself.
ir::Value PrepareArgument(Compiler &compiler, ir::Value arg_value,
                          type::QualType arg_qt, type::QualType param_qt);
base::untyped_buffer PrepareArgument(Compiler &c,
                                     base::untyped_buffer_view constant,
                                     ast::Expression const *expr,
                                     type::QualType param_qt);

ir::ArgumentBuffer EmitConstantArgumentBuffer(
    Compiler &c, absl::Span<ast::Call::Argument const> args);

core::Arguments<type::Typed<size_t>> EmitConstantArguments(
    Compiler &c, absl::Span<ast::Call::Argument const> args,
    base::untyped_buffer &buffer);

void EmitCall(
    Compiler &compiler, ast::Expression const *callee,
    base::untyped_buffer_view constant_buffer,
    core::Arguments<type::Typed<size_t>> const &constant_argument_indices,
    absl::Span<ast::Call::Argument const> arg_exprs,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to);

}  // namespace compiler

#endif  // ICARUS_IR_EMIT_COMMON_H
