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

// Returns A function which can be executed to complete the data-complete struct
// type pointed to by `s`.
std::optional<ir::CompiledFn> StructCompletionFn(
    CompilationDataReference data, type::Struct *s,
    absl::Span<ast::Declaration const> fields);

// Makes space for every local variable in this stack frame, but does not
// initialize any such object.
void MakeAllStackAllocations(Compiler &compiler, ast::Scope const *scope);

// Emits IR for each statement in `stmts`.
void EmitIrForStatements(Compiler &compiler, ast::Scope const *scope,
                         base::PtrSpan<ast::Node const> stmts);

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

// Requires that the last value in `buffer` has type `FromType`, and replaces it
// with that value cast to `ToType`.
template <typename FromType, typename ToType>
void EmitCast(IrBuilder &builder, ir::PartialResultBuffer &buffer) {
  if constexpr (base::meta<FromType> == base::meta<ToType>) {
    return;
  } else if constexpr (base::meta<ToType> == base::meta<ir::Integer>) {
    auto result = buffer.back().get<ToType>().value();
    buffer.pop_back();
    buffer.append(ir::Integer(result));
  } else {
    auto result =
        builder.CurrentBlock()->Append(ir::CastInstruction<ToType(FromType)>{
            .value  = buffer.back().template get<FromType>(),
            .result = builder.CurrentGroup()->Reserve(),
        });
    buffer.pop_back();
    buffer.append(result);
  }
}

// Requires that the last value in `buffer` has type `from`, and replaces it
// with that value cast to `to`.
void EmitCast(IrBuilder &builder, type::Type from, type::Type to,
                  ir::PartialResultBuffer &buffer);

void EmitCast(Compiler &c, type::Typed<ast::Expression const *> node,
              type::Type to, ir::PartialResultBuffer &buffer);

ir::PartialResultBuffer EmitCast(Compiler &c,
                                 type::Typed<ast::Expression const *> node,
                                 type::Type to);

// If the type `t` is not big, creates a new register referencing the value (or
// register) held in `value`. If `t` is big, `value` is either another register
// or the address of the big value and a new register referencing that address
// (or register) is created.
ir::Reg RegisterReferencing(IrBuilder &builder, type::Type t,
                            ir::PartialResultRef const &value);
ir::Reg PtrFix(IrBuilder &builder, ir::RegOr<ir::addr_t> addr,
               type::Type desired_type);


}  // namespace compiler

#endif  // ICARUS_IR_EMIT_COMMON_H
