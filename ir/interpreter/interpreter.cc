#include "ir/interpreter/interpreter.h"

#include <utility>

#include "absl/cleanup/cleanup.h"
#include "base/macros.h"
#include "core/arch.h"
#include "core/bytes.h"
#include "core/type_contour.h"
#include "ir/instruction/base.h"
#include "ir/interpreter/architecture.h"

namespace ir {
namespace {

// Computes the amount of space needed in this stack frame for all stack
// allocations.
core::Bytes StackFrameSize(Subroutine const& subroutine) {
  core::Bytes total;
  subroutine.for_each_alloc(
      ::interpreter::kArchitecture, [&](core::TypeContour t, Reg) {
        total = core::FwdAlign(total, t.alignment()) + t.bytes();
      });
  return total;
}

}  // namespace

BasicBlock const* InterpretInstruction(JumpCmd const& jump,
                                       interpreter::StackFrame& frame) {
  return jump.Visit([&](auto const& j) -> BasicBlock const* {
    constexpr auto jump_type = base::meta<std::decay_t<decltype(j)>>;
    if constexpr (jump_type == base::meta<ir::JumpCmd::UnreachableJump>) {
      frame.FatalError("Unreachable jump encountered");
      return nullptr;
    } else if constexpr (jump_type == base::meta<ir::JumpCmd::RetJump>) {
      return nullptr;
    } else if constexpr (jump_type == base::meta<ir::JumpCmd::BlockJump>) {
      NOT_YET();
    } else if constexpr (jump_type == base::meta<ir::JumpCmd::UncondJump>) {
      return j.block;
    } else if constexpr (jump_type == base::meta<ir::JumpCmd::CondJump>) {
      return frame.resolve<bool>(j.reg) ? j.true_block : j.false_block;
    } else {
      static_assert(base::always_false(jump_type));
    }
  });
}

namespace interpreter {

bool Interpreter::operator()(Subroutine const& subroutine,
                             CompleteResultBuffer const& arguments,
                             CompleteResultBuffer& result) {
  size_t num_outputs = 1;
  if (auto const* rt = subroutine.type()->if_as<type::ReturningType>()) {
    absl::Span outputs = rt->return_types();
    num_outputs        = outputs.size();
    for (type::Type t : outputs) {
      result.append_slot(t.bytes(::interpreter::kArchitecture).value());
    }
  }

  auto& frame = frames_.emplace_back(
      StackFrame::Summary{.required_stack_space  = StackFrameSize(subroutine),
                          .num_parameters        = subroutine.num_args(),
                          .num_registers         = subroutine.num_regs(),
                          .num_outputs           = num_outputs,
                          .num_stack_allocations = subroutine.num_allocs()},
      fatal_error_);
  absl::Cleanup c = [&] { frames_.pop_back(); };

  for (size_t i = 0; i < arguments.num_entries(); ++i) {
    base::untyped_buffer_view argument = arguments[i].raw();
    frame.set_raw(ir::Reg::Parameter(i), argument.data(), argument.size());
  }

  LOG("", "%u %u", num_outputs, result.num_entries());
  for (size_t i = 0; i < result.num_entries(); ++i) {
    frame.set(ir::Reg::Output(i), result[i].raw().data());
  }

  BasicBlock const* current  = subroutine.entry();
  BasicBlock const* previous = nullptr;
  while (current) {
    BasicBlock const* next = InterpretBasicBlock(*current);
    previous               = std::exchange(current, next);
  }

  return fatal_error_.empty();
}

BasicBlock const* Interpreter::InterpretBasicBlock(BasicBlock const& current) {
  for (auto const& instruction : current.instructions()) {
    if (not instruction) { continue; }
    bool ok = ::ir::InterpretInstruction(instruction, frames_.back());
    if (not ok) ICARUS_PREDICT_FALSE { break; }
  }

  return ir::InterpretInstruction(current.jump(), frames_.back());
}

std::optional<CompleteResultBuffer> Interpret(
    Subroutine const& subroutine, CompleteResultBuffer const& arguments) {
  Interpreter interpreter;
  CompleteResultBuffer output;
  if (interpreter(subroutine, arguments, output)) { return output; }
  return std::nullopt;
}

}  // namespace interpreter
}  // namespace ir
