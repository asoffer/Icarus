#include "ir/interpreter/interpreter.h"

#include <utility>

#include "absl/cleanup/cleanup.h"
#include "absl/strings/str_cat.h"
#include "base/macros.h"
#include "core/arch.h"
#include "core/bytes.h"
#include "core/type_contour.h"
#include "ir/instruction/base.h"
#include "ir/interpreter/ffi.h"

namespace ir {
namespace {

// Computes the amount of space needed in this stack frame for all stack
// allocations.
core::Bytes StackFrameSize(Subroutine const& subroutine) {
  core::Bytes total;
  subroutine.for_each_alloc(core::Host, [&](core::TypeContour t, Reg) {
    total = core::FwdAlign(total, t.alignment()) + t.bytes();
  });
  return total;
}

}  // namespace

BasicBlock const* InterpretInstruction(interpreter::Interpreter& interpreter,
                                       JumpCmd const& jump) {
  return jump.Visit([&](auto const& j) -> BasicBlock const* {
    constexpr auto jump_type = base::meta<std::decay_t<decltype(j)>>;
    if constexpr (jump_type == base::meta<JumpCmd::UnreachableJump>) {
      interpreter.FatalError("Unreachable jump encountered");
      return nullptr;
    } else if constexpr (jump_type == base::meta<JumpCmd::RetJump>) {
      interpreter.pop_frame();
      return nullptr;
    } else if constexpr (jump_type == base::meta<JumpCmd::BlockJump>) {
      // TODO: Out(0) may not be sufficient.
      auto& blocks = *ASSERT_NOT_NULL(reinterpret_cast<std::vector<Block>*>(
          interpreter.frame().resolve<addr_t>(Reg::Output(0))));
      blocks.push_back(j.block);
      return j.after;
    } else if constexpr (jump_type == base::meta<JumpCmd::UncondJump>) {
      return j.block;
    } else if constexpr (jump_type == base::meta<JumpCmd::CondJump>) {
      return interpreter.frame().resolve<bool>(j.reg) ? j.true_block
                                                      : j.false_block;
    } else {
      static_assert(base::always_false(jump_type));
    }
  });
}

namespace interpreter {

bool Interpreter::operator()() {
  ASSERT(instruction_pointers_.size() != 0u);
  while (not instruction_pointers_.empty()) {
    BasicBlock const* next = InterpretFromInstructionPointer();
    if (not fatal_error_.empty()) { return false; }
    if (next) { instruction_pointers_.back().update(next); }
  }
  return true;
}

BasicBlock const* Interpreter::InterpretFromInstructionPointer() {
  ASSERT(instruction_pointers_.size() != 0);
  size_t ip_size                             = instruction_pointers_.size() - 1;
  auto [basic_block, iter, prev_basic_block] = instruction_pointers_.back();
  auto end_iter = basic_block->instructions().end();
  for (; iter != end_iter; ++iter) {
    auto const& instruction = *iter;
    if (not instruction) { continue; }

    if (not ir::InterpretInstruction(*this, instruction)) {
      instruction_pointers_[ip_size].iterator = std::next(iter);
      return nullptr;
    }
  }

  return ir::InterpretInstruction(*this, basic_block->jump());
}

bool Interpreter::push_frame(Subroutine const* subroutine,
                             CompleteResultBuffer const& arguments,
                             absl::Span<addr_t const> outputs) {
  ASSERT(subroutine != nullptr);
  ASSERT(subroutine->blocks().size() != 0);
  ASSERT(subroutine->type()->parameters().size() == arguments.num_entries());

  auto& frame = frames_.emplace_back(
      StackFrame::Summary{.required_stack_space  = StackFrameSize(*subroutine),
                          .num_parameters        = subroutine->num_args(),
                          .num_registers         = subroutine->num_regs(),
                          .num_outputs           = outputs.size(),
                          .num_stack_allocations = subroutine->num_allocs()});

  subroutine->for_each_alloc(core::Host, [&, next_reg_loc = core::Bytes(),
                                          i = 0](core::TypeContour tc,
                                                 Reg) mutable {
    next_reg_loc = core::FwdAlign(next_reg_loc, tc.alignment());
    frame.set(Reg::StackAllocation(i++), frame.frame() + next_reg_loc.value());
    next_reg_loc += tc.bytes();
  });

  for (size_t i = 0; i < arguments.num_entries(); ++i) {
    base::untyped_buffer_view argument = arguments[i].raw();
    frame.set_raw(Reg::Parameter(i), argument.data(), argument.size());
  }

  for (size_t i = 0; i < outputs.size(); ++i) {
    frame.set(Reg::Output(i), outputs[i]);
  }

  instruction_pointers_.emplace_back(subroutine);
  return false;
}

bool Interpreter::push_frame(Fn f, CompleteResultBuffer const& arguments,
                             absl::Span<addr_t const> outputs) {
  if (f.module() == ModuleId::Foreign()) {
    ASSERT(outputs.size() <= 1);
    bool result = InvokeForeignFunction(
        *context_.ForeignFunctionType(f), context_.ForeignFunctionPointer(f),
        arguments, outputs.empty() ? nullptr : outputs[0]);
    if (not result) {
      FatalError(absl::StrCat(
          "Fatal error encountered when trying to call foreign function ",
          f.local().value()));
    }
    return result;
  } else {
    auto& subroutine = *ASSERT_NOT_NULL(context_.Function(f).subroutine);
    return push_frame(&subroutine, arguments, outputs);
  }
}

void Interpreter::pop_frame() {
  instruction_pointers_.pop_back();
  frames_.pop_back();
}

std::optional<CompleteResultBuffer> Interpret(
    module::SharedContext const& context, Subroutine const& subroutine,
    CompleteResultBuffer const& arguments) {
  CompleteResultBuffer output_buffer;

  Interpreter interpreter(&context);

  size_t num_outputs = 1;
  if (auto const* rt = subroutine.type()->if_as<type::ReturningType>()) {
    absl::Span outputs = rt->return_types();
    num_outputs        = outputs.size();
    for (type::Type t : outputs) {
      output_buffer.append_slot(t.bytes(core::Host).value());
    }
  }

  std::vector<addr_t> addrs;
  addrs.reserve(num_outputs);
  for (size_t i = 0; i < num_outputs; ++i) {
    addrs.push_back(const_cast<addr_t>(output_buffer[i].raw().data()));
  }
  interpreter.push_frame(&subroutine, arguments, addrs);

  if (not interpreter()) { return std::nullopt; }
  return output_buffer;
}

std::optional<CompleteResultBuffer> Interpret(
    module::SharedContext const& context, Fn f,
    CompleteResultBuffer const& arguments) {
  return Interpret(context, *ASSERT_NOT_NULL(context.Function(f).subroutine),
                   arguments);
}

}  // namespace interpreter
}  // namespace ir
