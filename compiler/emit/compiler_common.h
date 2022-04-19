#ifndef ICARUS_IR_EMIT_COMPILER_COMMON_H
#define ICARUS_IR_EMIT_COMPILER_COMMON_H

#include <concepts>
#include <optional>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope.h"
#include "base/ptr_span.h"
#include "compiler/compiler.h"
#include "compiler/instantiate.h"
#include "ir/instruction/compare.h"
#include "ir/instruction/instructions.h"
#include "ir/subroutine.h"
#include "ir/value/result_buffer.h"
#include "type/struct.h"

namespace compiler {

// Emits IR for each statement in `stmts`.
void EmitIrForStatements(Compiler &compiler, ast::Scope const *scope,
                         base::PtrSpan<ast::Node const> stmts);

void AppendToPartialResultBuffer(Compiler &c, type::QualType qt,
                                 ast::Expression const &expr,
                                 ir::PartialResultBuffer &buffer);

// Note: The `CompleteResultRef`s passed in `constant_arguments` must refer to a
// buffer that outlives the call to this function.
void EmitCall(Compiler &compiler, CallMetadata::callee_locator_t callee,
              core::Arguments<type::Typed<ir::CompleteResultRef>> const
                  &constant_arguments,
              absl::Span<ast::Call::Argument const> arg_exprs,
              absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to);

core::Arguments<type::Typed<ir::CompleteResultRef>> EmitConstantArguments(
    Compiler &c, absl::Span<ast::Call::Argument const> args,
    ir::CompleteResultBuffer &buffer);

void EmitArguments(
    Compiler &c, core::Parameters<type::QualType> const &param_qts,
    core::Parameters<ast::Expression const *> const &defaults,
    absl::Span<ast::Call::Argument const> arg_exprs,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &constants,
    ir::PartialResultBuffer &buffer);

void EmitCast(Compiler &c, type::Typed<ast::Expression const *> node,
              type::Type to, ir::PartialResultBuffer &buffer);

ir::PartialResultBuffer EmitCast(Compiler &c,
                                 type::Typed<ast::Expression const *> node,
                                 type::Type to);

}  // namespace compiler

#endif  // ICARUS_IR_EMIT_COMPILER_COMMON_H
