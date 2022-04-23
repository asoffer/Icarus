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

bool InterpretInstruction(Inst const&, interpreter::StackFrame&);

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

}  // namespace ir

namespace ir::interpreter {
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

std::byte* IncrementedBy(std::byte* p, size_t register_count) {
  return p + Interpreter::register_size * register_count;
}

std::array<std::byte*, 4> MakeStarts(Subroutine const& subroutine,
                                     std::byte* start) {
  size_t num_outputs = 1;
  if (auto const* rt = subroutine.type()->if_as<type::ReturningType>()) {
    num_outputs = rt->return_types().size();
  }

  std::byte* value     = start;
  std::byte* output    = IncrementedBy(value, subroutine.num_regs());
  std::byte* parameter = IncrementedBy(output, num_outputs);
  std::byte* alloc     = IncrementedBy(parameter, subroutine.num_args());
  return std::array<std::byte*, 4>{value, output, parameter, alloc};
}

}  // namespace

StackFrame::StackFrame(Subroutine const& subroutine, size_t register_size,
                       std::string& fatal_error)
    : frame_(
          base::untyped_buffer::MakeFull(StackFrameSize(subroutine).value())),
      registers_(base::untyped_buffer::MakeFull(subroutine.num_regs() *
                                                register_size)),
      starts_(MakeStarts(subroutine, registers_.raw(0))),
      fatal_error_(fatal_error) {}

void Interpreter::operator()(Subroutine const& subroutine) {
  frames_.emplace_back(subroutine, register_size, fatal_error_);
  absl::Cleanup c = [&] { frames_.pop_back(); };

  BasicBlock const* current  = subroutine.entry();
  BasicBlock const* previous = nullptr;
  while (current) {
    BasicBlock const* next = InterpretBasicBlock(*current);
    previous               = std::exchange(current, next);
  }
}

std::byte* StackFrame::find(Reg r) {
  auto kind = static_cast<std::underlying_type_t<Reg::Kind>>(r.kind());
  return starts_[kind] + Interpreter::register_size * r.raw_value();
}

std::byte const* StackFrame::find(Reg r) const {
  auto kind = static_cast<std::underlying_type_t<Reg::Kind>>(r.kind());
  return starts_[kind] + Interpreter::register_size * r.raw_value();
}

BasicBlock const* Interpreter::InterpretBasicBlock(BasicBlock const& current) {
  for (auto const& instruction : current.instructions()) {
    if (not instruction) { continue; }
    bool ok = ::ir::InterpretInstruction(instruction, frames_.back());
    if (not ok) ICARUS_PREDICT_FALSE { break; }
  }

  return ir::InterpretInstruction(current.jump(), frames_.back());
}

}  // namespace ir::interpreter
