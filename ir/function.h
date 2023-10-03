#ifndef ICARUS_IR_FUNCTION_H
#define ICARUS_IR_FUNCTION_H

#include <cstdio>
#include <string_view>

#include "jasmin/function.h"
#include "jasmin/instruction.h"
#include "jasmin/instructions/core.h"
#include "jasmin/value.h"

namespace ic {

struct PushFunction : jasmin::StackMachineInstruction<PushFunction> {
  static std::string_view name() { return "push-function"; }

  static constexpr void execute(jasmin::ValueStack& value_stack,
                                jasmin::Value v) {
    value_stack.push(v);
  }
};

struct PrintHelloWorld : jasmin::StackMachineInstruction<PrintHelloWorld> {
  static void execute() { std::puts("Hello, world!"); }
};

using InstructionSet =
    jasmin::MakeInstructionSet<jasmin::Push, PushFunction, jasmin::Drop,
                               PrintHelloWorld>;
using IrFunction = jasmin::Function<InstructionSet>;

}  // namespace ic

#endif  // ICARUS_IR_FUNCTION_H
