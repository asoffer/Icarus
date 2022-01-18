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
#include "ir/instruction/compare.h"
#include "ir/instruction/instructions.h"
#include "ir/value/result_buffer.h"
#include "type/struct.h"

namespace compiler {

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
void EmitCast(GroupBlockReference &ref, ir::PartialResultBuffer &buffer) {
  if constexpr (base::meta<FromType> == base::meta<ToType>) {
    return;
  } else if constexpr (base::meta<ToType> == base::meta<ir::Integer>) {
    auto result = buffer.back().get<ToType>().value();
    buffer.pop_back();
    buffer.append(ir::Integer(result));
  } else {
    auto result = ref.block->Append(ir::CastInstruction<ToType(FromType)>{
        .value  = buffer.back().template get<FromType>(),
        .result = ref.group->Reserve(),
    });
    buffer.pop_back();
    buffer.append(result);
  }
}

// Requires that the last value in `buffer` has type `from`, and replaces it
// with that value cast to `to`.
void EmitCast(GroupBlockReference &ref, type::Type from, type::Type to,
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
ir::Reg RegisterReferencing(GroupBlockReference current, type::Type t,
                            ir::PartialResultRef const &value);
ir::Reg PtrFix(GroupBlockReference current, ir::RegOr<ir::addr_t> addr,
               type::Type desired_type);

// Usually it is sufficient to determine all the inputs to a phi instruction
// upfront, but sometimes it is useful to construct a phi instruction without
// having set its inputs.
//
// TODO: Right now we are relying on the fact that Inst stores values on the
// heap, but this may not always be the case.
template <typename T>
ir::PhiInstruction<T> *PhiInst(GroupBlockReference ref) {
  ir::PhiInstruction<T> inst;
  inst.result = ref.group->Reserve();
  ref.block->Append(std::move(inst));
  return &ref.block->instructions().back().template as<ir::PhiInstruction<T>>();
}

void OnEachArrayElement(GroupBlockReference &ref, type::Array const *t,
                        ir::Reg array_reg, std::invocable<ir::Reg> auto &&fn) {
  auto *data_ptr_type = type::Ptr(t->data_type());

  ir::Reg end_ptr =
      ref.block->Append(ir::PtrIncrInstruction{.addr   = array_reg,
                                               .index  = t->length().value(),
                                               .ptr    = data_ptr_type,
                                               .result = ref.group->Reserve()});
  auto *start_block = ref.block;
  auto *loop_body   = ref.group->AppendBlock();
  auto *land_block  = ref.group->AppendBlock();
  auto *cond_block  = ref.group->AppendBlock();

  ref.block->set_jump(ir::JumpCmd::Uncond(cond_block));

  ref.block = cond_block;
  auto *phi = PhiInst<ir::addr_t>(ref);

  ir::Reg condition = ref.block->Append(ir::EqInstruction<ir::addr_t>{
      .lhs = phi->result, .rhs = end_ptr, .result = ref.group->Reserve()});
  ref.block->set_jump(ir::JumpCmd::Cond(condition, land_block, loop_body));

  ref.block = loop_body;
  fn(phi->result);
  ir::Reg next =
      ref.block->Append(ir::PtrIncrInstruction{.addr   = phi->result,
                                               .index  = 1,
                                               .ptr    = data_ptr_type,
                                               .result = ref.group->Reserve()});
  ref.block->set_jump(ir::JumpCmd::Uncond(cond_block));

  phi->add(start_block, array_reg);
  phi->add(ref.block, next);

  ref.block = land_block;
}

}  // namespace compiler

#endif  // ICARUS_IR_EMIT_COMMON_H
