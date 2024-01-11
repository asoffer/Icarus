#ifndef ICARUS_IR_FUNCTION_H
#define ICARUS_IR_FUNCTION_H

#include <span>
#include <string_view>

#include "common/identifier.h"
#include "common/integer.h"
#include "common/interface.h"
#include "common/pattern.h"
#include "ir/function_id.h"
#include "jasmin/core/function.h"
#include "jasmin/core/instruction.h"
#include "jasmin/core/value.h"
#include "jasmin/instructions/arithmetic.h"
#include "jasmin/instructions/bool.h"
#include "jasmin/instructions/common.h"
#include "jasmin/instructions/compare.h"
#include "jasmin/instructions/stack.h"
#include "type/basic.h"
#include "type/function.h"
#include "type/opaque.h"
#include "type/parameters.h"
#include "type/pointer.h"
#include "type/refinement.h"

namespace ic {

struct Store : jasmin::Instruction<Store> {
  static void consume(std::span<jasmin::Value, 2> input, uint8_t size) {
    jasmin::Value value = input[0];
    void* location      = input[1].as<void*>();
    jasmin::Value::Store(value, location, size);
  }
  static constexpr std::string_view debug() { return "store"; }
};

struct PushFunction : jasmin::Instruction<PushFunction> {
  static std::string_view name() { return "push-function"; }

  static jasmin::Value execute(std::span<jasmin::Value, 0>, jasmin::Value v) {
    return v;
  }
};

struct PushNull : jasmin::Instruction<PushNull> {
  static std::string_view name() { return "push-null"; }

  static void* execute(std::span<jasmin::Value, 0>) { return nullptr; }
};

struct PushPointer : jasmin::Instruction<PushPointer> {
  static std::string_view name() { return "push-pointer"; }

  static jasmin::Value execute(std::span<jasmin::Value, 0>, jasmin::Value v) {
    return v;
  }
};

struct PushStringLiteral : jasmin::Instruction<PushStringLiteral> {
  static std::string_view name() { return "push-string-literal"; }

  static std::array<jasmin::Value, 2> execute(std::span<jasmin::Value, 0>,
                                              char const* data, size_t length) {
    return {jasmin::Value(data), jasmin::Value(length)};
  }
};

struct PushType : jasmin::Instruction<PushType> {
  static std::string_view name() { return "push-type"; }
  static type::Type execute(std::span<jasmin::Value, 0>, type::Type t) {
    return t;
  }
};

struct RegisterForeignFunction : jasmin::Instruction<RegisterForeignFunction> {
  static std::string_view name() { return "register-foreign-function"; }

  static jasmin::Value consume(std::span<jasmin::Value, 3> inputs);
};

struct InvokeForeignFunction : jasmin::Instruction<InvokeForeignFunction> {
  static std::string_view name() { return "invoke-foreign-function"; }

  static void consume(std::span<jasmin::Value> input,
                      std::span<jasmin::Value> output, type::FunctionType type,
                      void const* fn_ptr);
};

struct TypeKind : jasmin::Instruction<TypeKind> {
  static std::string_view name() { return "type-kind"; }

  static type::Type::Kind consume(std::span<jasmin::Value, 1> inputs) {
    return inputs[0].as<type::Type>().kind();
  }
};

struct ConstructParametersType : jasmin::Instruction<ConstructParametersType> {
  static std::string_view name() { return "construct-parameters-type"; }

  static void consume(std::span<jasmin::Value> inputs,
                      std::span<jasmin::Value> outputs) {
    std::vector<type::ParametersType::Parameter> parameters;
    parameters.reserve(inputs.size());
    for (jasmin::Value value : inputs) {
      parameters.emplace_back().type = value.as<type::Type>();
    }
    outputs[0] = type::Type(type::Parameters(std::move(parameters)));
  }
};

struct ConstructInterface : jasmin::Instruction<ConstructInterface> {
  static std::string_view name() { return "construct-interface"; }

  static void consume(std::span<jasmin::Value> inputs,
                      std::span<jasmin::Value> outputs);
};

struct ConstructRefinementType : jasmin::Instruction<ConstructRefinementType> {
  static std::string_view name() { return "construct-parameters-type"; }

  static type::Type consume(std::span<jasmin::Value, 2> inputs) {
    return type::Refinement(inputs[0].as<type::Type>(),
                            inputs[1].as<Pattern>());
  }
};

struct ConstructFunctionType : jasmin::Instruction<ConstructFunctionType> {
  static std::string_view name() { return "construct-function-type"; }

  static type::Type consume(std::span<jasmin::Value, 2> inputs);
};

struct ConstructOpaqueType : jasmin::Instruction<ConstructOpaqueType> {
  static std::string_view name() { return "construct-opaque-type"; }

  static type::Type execute(std::span<jasmin::Value, 0>) {
    return type::Opaque();
  }
};

struct ConstructPointerType : jasmin::Instruction<ConstructPointerType> {
  static std::string_view name() { return "construct-pointer-type"; }

  static type::Type consume(std::span<jasmin::Value, 1> inputs) {
    return type::Ptr(inputs[0].as<type::Type>());
  }
};

struct ConstructBufferPointerType
    : jasmin::Instruction<ConstructBufferPointerType> {
  static std::string_view name() { return "construct-buffer-pointer-type"; }

  static type::Type consume(std::span<jasmin::Value, 1> inputs) {
    return type::BufPtr(inputs[0].as<type::Type>());
  }
};

struct ConstructSliceType : jasmin::Instruction<ConstructSliceType> {
  static std::string_view name() { return "construct-slice-type"; }

  static type::Type consume(std::span<jasmin::Value, 1> inputs) {
    return type::Slice(inputs[0].as<type::Type>());
  }
};

struct NoOp : jasmin::Instruction<NoOp> {
  static std::string_view name() { return "no-op"; }
  static void execute(std::span<jasmin::Value, 0>) {}
};

struct AddPointer : jasmin::Instruction<AddPointer> {
  static std::byte const* consume(std::span<jasmin::Value, 2> inputs) {
    return inputs[0].as<std::byte const*>() + inputs[1].as<uint64_t>();
  }
};

struct AsciiEncode : jasmin::Instruction<AsciiEncode> {
  static void consume(std::span<jasmin::Value, 1> inputs) {
    inputs[0] = static_cast<char>(inputs[0].as<uint8_t>());
  }
};

struct AsciiDecode : jasmin::Instruction<AsciiDecode> {
  static void consume(std::span<jasmin::Value, 1> inputs) {
    inputs[0] = static_cast<uint8_t>(inputs[0].as<char>());
  }
};

struct LoadProgramArguments : jasmin::Instruction<LoadProgramArguments> {
  static std::array<jasmin::Value, 2> execute(std::span<jasmin::Value, 0>);
};

struct CheckInterfaceSatisfaction
    : jasmin::Instruction<CheckInterfaceSatisfaction> {
  static bool consume(std::span<jasmin::Value, 1>, Interface);
};

struct Rotate : jasmin::Instruction<Rotate> {
  static void execute(std::span<jasmin::Value> values,
                      std::span<jasmin::Value>) {
    NTH_REQUIRE((v.harden), values.size() >= 1);
    jasmin::Value v = values[0];
    for (size_t i = 1; i < values.size(); ++i) { values[i - 1] = values[i]; }
    values.back() = v;
  }
};

using InstructionSet = jasmin::MakeInstructionSet<
    jasmin::Push, PushFunction, PushStringLiteral, PushType, PushPointer,
    PushNull, jasmin::Equal<type::Type::Kind>, Rotate, ConstructOpaqueType,
    ConstructPointerType, ConstructBufferPointerType, ConstructFunctionType,
    ConstructParametersType, ConstructSliceType, ConstructInterface,
    RegisterForeignFunction, InvokeForeignFunction, jasmin::Not, NoOp, Store,
    jasmin::Load, jasmin::StackAllocate, jasmin::StackOffset,
    jasmin::Add<int64_t>, jasmin::Subtract<int64_t>, jasmin::Multiply<int64_t>,
    jasmin::Mod<int64_t>, jasmin::Equal<int64_t>, jasmin::LessThan<int64_t>,
    AddPointer, LoadProgramArguments, jasmin::Duplicate, AsciiEncode,
    AsciiDecode, jasmin::Drop, jasmin::Swap, TypeKind, jasmin::Negate<int8_t>,
    jasmin::Negate<int16_t>, jasmin::Negate<int32_t>, jasmin::Negate<int64_t>,
    jasmin::Negate<Integer>, jasmin::Negate<float>, jasmin::Negate<double>,
    ConstructRefinementType, CheckInterfaceSatisfaction>;
using IrFunction = jasmin::Function<InstructionSet>;

void Extend(Interface intf, type::Type t);

}  // namespace ic

#endif  // ICARUS_IR_FUNCTION_H
