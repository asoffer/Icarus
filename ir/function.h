#ifndef ICARUS_IR_FUNCTION_H
#define ICARUS_IR_FUNCTION_H

#include <cstdio>
#include <queue>
#include <string_view>

#include "common/identifier.h"
#include "jasmin/function.h"
#include "jasmin/instruction.h"
#include "jasmin/instructions/compare.h"
#include "jasmin/instructions/core.h"
#include "jasmin/value.h"
#include "jasmin/value_stack.h"
#include "type/type.h"

namespace ic {

struct PushFunction : jasmin::StackMachineInstruction<PushFunction> {
  static std::string_view name() { return "push-function"; }

  static void execute(jasmin::ValueStack& value_stack, jasmin::Value v) {
    value_stack.push(v);
  }
};

struct PushStringLiteral : jasmin::StackMachineInstruction<PushStringLiteral> {
  static std::string_view name() { return "push-string-literal"; }

  static void execute(jasmin::ValueStack& value_stack, char const* data,
                      size_t length) {
    value_stack.push(data);
    value_stack.push(length);
  }
};

struct PushType : jasmin::StackMachineInstruction<PushType> {
  static std::string_view name() { return "push-type"; }

  static void execute(jasmin::ValueStack& value_stack, type::Type t) {
    value_stack.push(t);
  }
};

struct RegisterForeignFunction
    : jasmin::StackMachineInstruction<RegisterForeignFunction> {
  static std::string_view name() { return "register-foreign-function"; }

  static void execute(jasmin::ValueStack& value_stack);
};


struct InvokeForeignFunction
    : jasmin::StackMachineInstruction<InvokeForeignFunction> {
  static std::string_view name() { return "invoke-foreign-function"; }

  static void execute(jasmin::ValueStack& value_stack, type::FunctionType type,
                      void const* fn_ptr);
};

struct TypeKind : jasmin::StackMachineInstruction<TypeKind> {
  static std::string_view name() { return "type-kind"; }

  static type::Type::Kind execute(type::Type t) { return t.kind(); }
};

struct ConstructFunctionType
    : jasmin::StackMachineInstruction<ConstructFunctionType> {
  static std::string_view name() { return "construct-function-type"; }

  static type::Type execute(type::Type parameter, type::Type return_type) {
    return type::Function(
        type::Parameters(std::vector<type::ParametersType::Parameter>{
            {.name = Identifier("").value(), .type = parameter}}),
        std::vector{return_type});
  }
};

struct ConstructPointerType
    : jasmin::StackMachineInstruction<ConstructPointerType> {
  static std::string_view name() { return "construct-pointer-type"; }

  static type::Type execute(type::Type pointee) { return type::Ptr(pointee); }
};

struct ConstructBufferPointerType
    : jasmin::StackMachineInstruction<ConstructBufferPointerType> {
  static std::string_view name() { return "construct-buffer-pointer-type"; }

  static type::Type execute(type::Type pointee) {
    return type::BufPtr(pointee);
  }
};

struct Print : jasmin::StackMachineInstruction<Print> {
  static void execute(size_t length, char const* p) {
    std::fprintf(stderr, "%*s", static_cast<int>(length), p);
  }
};

struct Rotate : jasmin::StackMachineInstruction<Rotate> {
  static void execute(jasmin::ValueStack& value_stack, size_t n) {
    NTH_REQUIRE((v.harden), n > 1);
    std::queue<jasmin::Value> q;
    for (size_t i = 1; i < n; ++i) { q.push(value_stack.pop_value()); }
    jasmin::Value v = value_stack.pop_value();
    while (not q.empty()) {
      value_stack.push(q.front());
      q.pop();
    }
    value_stack.push(v);
  }
};

using InstructionSet = jasmin::MakeInstructionSet<
    jasmin::Push, PushFunction, PushStringLiteral, PushType, jasmin::Drop,
    TypeKind, jasmin::Equal<type::Type::Kind>, Print, Rotate,
    ConstructPointerType, ConstructBufferPointerType, ConstructFunctionType,
    jasmin::Swap, RegisterForeignFunction, InvokeForeignFunction>;
using IrFunction = jasmin::Function<InstructionSet>;

std::deque<std::pair<type::FunctionType, IrFunction>>& ForeignFunctions();

}  // namespace ic

#endif  // ICARUS_IR_FUNCTION_H
