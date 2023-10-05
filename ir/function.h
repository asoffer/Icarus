#ifndef ICARUS_IR_FUNCTION_H
#define ICARUS_IR_FUNCTION_H

#include <cstdio>
#include <queue>
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
    value_stack.push(length);
    value_stack.push(data);
  }
};

struct TypeKind : jasmin::StackMachineInstruction<TypeKind> {
  static std::string_view name() { return "type-kind"; }

  static constexpr type::Type::Kind execute(type::Type t) { return t.kind(); }
};

struct Print : jasmin::StackMachineInstruction<Print> {
  static void execute(char const* p, size_t length) {
    std::printf("%*s", static_cast<int>(length), p);
  }
};

struct Rotate : jasmin::StackMachineInstruction<Rotate> {
  static void execute(jasmin::ValueStack& value_stack, size_t n) {
    NTH_REQUIRE((v.harden), n > 1);
    std::queue<jasmin::Value> q;
    for (size_t i = 1; i < n; ++i) {
      q.push(value_stack.pop_value());
    }
    jasmin::Value v = value_stack.pop_value();
    while (not q.empty()) {
      value_stack.push(q.front());
      q.pop();
    }
    value_stack.push(v);
  }
};

using InstructionSet =
    jasmin::MakeInstructionSet<jasmin::Push, PushFunction, PushStringLiteral,
                               jasmin::Drop, TypeKind,
                               jasmin::Equal<type::Type::Kind>, Print, Rotate>;
using IrFunction = jasmin::Function<InstructionSet>;

}  // namespace ic

#endif  // ICARUS_IR_FUNCTION_H
