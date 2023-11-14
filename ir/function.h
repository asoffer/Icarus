#ifndef ICARUS_IR_FUNCTION_H
#define ICARUS_IR_FUNCTION_H

#include <cstdio>
#include <queue>
#include <string_view>

#include "common/identifier.h"
#include "ir/function_id.h"
#include "jasmin/function.h"
#include "jasmin/instruction.h"
#include "jasmin/instructions/bool.h"
#include "jasmin/instructions/compare.h"
#include "jasmin/instructions/core.h"
#include "jasmin/instructions/arithmetic.h"
#include "jasmin/instructions/stack.h"
#include "jasmin/value.h"
#include "jasmin/value_stack.h"
#include "type/type.h"

namespace ic {

struct Store : jasmin::StackMachineInstruction<Store> {
  static void execute(jasmin::ValueStack& value_stack, uint8_t size) {
    void* location      = value_stack.pop<void*>();
    jasmin::Value value = value_stack.pop_value();
    jasmin::Value::Store(value, location, size);
  }
  static constexpr std::string_view debug() { return "store"; }
};

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

struct ConstructParametersType
    : jasmin::StackMachineInstruction<ConstructParametersType> {
  static std::string_view name() { return "construct-parameters-type"; }

  static void execute(jasmin::ValueStack& value_stack, size_t count) {
    std::vector<type::ParametersType::Parameter> parameters;
    parameters.resize(count, {.name = Identifier("").value()});
    for (int32_t i = count - 1; i >= 0; --i) {
      parameters[i].type = value_stack.pop<type::Type>();
    }
    value_stack.push(type::Type(type::Parameters(std::move(parameters))));
  }
};

struct ConstructFunctionType
    : jasmin::StackMachineInstruction<ConstructFunctionType> {
  static std::string_view name() { return "construct-function-type"; }

  static type::Type execute(type::Type parameter, type::Type return_type) {
    if (parameter.kind() == type::Type::Kind::Parameters) {
      return type::Function(parameter.AsParameters(), std::vector{return_type});
    } else {
      return type::Function(type::Parameters({{.name = Identifier("").value(),
                                               .type = parameter}}),
                            std::vector{return_type});
    }
  }
};

struct ConstructOpaqueType
    : jasmin::StackMachineInstruction<ConstructOpaqueType> {
  static std::string_view name() { return "construct-opaque-type"; }

  static type::Type execute() { return type::Opaque(); }
};

struct ConstructPointerType
    : jasmin::StackMachineInstruction<ConstructPointerType> {
  static std::string_view name() { return "construct-pointer-type"; }

  static type::Type execute(type::Type pointee) {
    return type::Ptr(pointee);
  }
};

struct ConstructBufferPointerType
    : jasmin::StackMachineInstruction<ConstructBufferPointerType> {
  static std::string_view name() { return "construct-buffer-pointer-type"; }

  static type::Type execute(type::Type pointee) {
    return type::BufPtr(pointee);
  }
};

struct ConstructSliceType
    : jasmin::StackMachineInstruction<ConstructSliceType> {
  static std::string_view name() { return "construct-slice-type"; }

  static type::Type execute(type::Type pointee) {
    return type::Slice(pointee);
  }
};

struct NoOp : jasmin::StackMachineInstruction<NoOp> {
  static std::string_view name() { return "no-op"; }
  static void execute(jasmin::ValueStack& vs) {}
};

struct Rotate : jasmin::StackMachineInstruction<Rotate> {
  static void execute(jasmin::ValueStack& value_stack, size_t n) {
    NTH_REQUIRE((v.harden), n >= 1);
    std::stack<jasmin::Value> stack;
    for (size_t i = 1; i < n; ++i) { stack.push(value_stack.pop_value()); }
    jasmin::Value v = value_stack.pop_value();
    while (not stack.empty()) {
      value_stack.push(stack.top());
      stack.pop();
    }
    value_stack.push(v);
  }
};

using InstructionSet = jasmin::MakeInstructionSet<
    jasmin::Push, PushFunction, PushStringLiteral, PushType, jasmin::Drop,
    TypeKind, jasmin::Equal<type::Type::Kind>, Rotate, ConstructOpaqueType,
    ConstructPointerType, ConstructBufferPointerType, ConstructFunctionType,
    ConstructParametersType, ConstructSliceType, jasmin::Swap,
    RegisterForeignFunction, InvokeForeignFunction, jasmin::Not, NoOp, Store,
    jasmin::Load, jasmin::StackAllocate, jasmin::StackOffset,
    jasmin::Add<int64_t>, jasmin::Subtract<int64_t>, jasmin::Multiply<int64_t>,
    jasmin::Mod<int64_t>, jasmin::Equal<int64_t>, jasmin::LessThan<int64_t>>;
using IrFunction = jasmin::Function<InstructionSet>;

std::deque<std::pair<type::FunctionType, IrFunction>>& ForeignFunctions();

std::pair<type::FunctionType, IrFunction> const& LookupForeignFunction(
    LocalFunctionId id);

}  // namespace ic

#endif  // ICARUS_IR_FUNCTION_H
