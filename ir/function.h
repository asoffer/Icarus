#ifndef ICARUS_IR_FUNCTION_H
#define ICARUS_IR_FUNCTION_H

#include <cstdio>
#include <string_view>

#include "jasmin/function.h"
#include "jasmin/instruction.h"
#include "jasmin/instructions/compare.h"
#include "jasmin/instructions/core.h"
#include "jasmin/value.h"
#include "type/type.h"

namespace ic {

struct PushFunction : jasmin::StackMachineInstruction<PushFunction> {
  static std::string_view name() { return "push-function"; }

  static constexpr void execute(jasmin::ValueStack& value_stack,
                                jasmin::Value v) {
    value_stack.push(v);
  }
};

struct PushStringLiteral : jasmin::StackMachineInstruction<PushStringLiteral> {
  static std::string_view name() { return "push-string-literal"; }

  static constexpr void execute(jasmin::ValueStack& value_stack,
                                char const* data, size_t length) {
    value_stack.push(data);
    value_stack.push(length);
  }
};

struct TypeKind : jasmin::StackMachineInstruction<TypeKind> {
  static std::string_view name() { return "type-kind"; }

  static constexpr type::Type::Kind execute(type::Type t) { return t.kind(); }
};

struct PrintHelloWorld : jasmin::StackMachineInstruction<PrintHelloWorld> {
  static void execute() { std::puts("Hello, world!"); }
};

using InstructionSet = jasmin::MakeInstructionSet<
    jasmin::Push, PushFunction, PushStringLiteral, jasmin::Drop, TypeKind,
    jasmin::Equal<type::Type::Kind>, PrintHelloWorld>;
using IrFunction = jasmin::Function<InstructionSet>;

}  // namespace ic

#endif  // ICARUS_IR_FUNCTION_H
