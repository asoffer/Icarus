#ifndef ICARUS_IR_FUNCTION_H
#define ICARUS_IR_FUNCTION_H

#include <span>
#include <string_view>

#include "jasmin/core/function_registry.h"
#include "common/identifier.h"
#include "common/integer.h"
#include "common/interface.h"
#include "common/pattern.h"
#include "common/string_literal.h"
#include "ir/function_id.h"
#include "jasmin/core/function.h"
#include "jasmin/core/input.h"
#include "jasmin/core/instruction.h"
#include "jasmin/core/output.h"
#include "jasmin/core/program_fragment.h"
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

struct PushNull : jasmin::Instruction<PushNull> {
  static void execute(jasmin::Input<>, jasmin::Output<void*> out) {
    out.set(nullptr);
  }
};

struct Store : jasmin::Instruction<Store> {
  static void consume(jasmin::Input<jasmin::Value, void*> input,
                      jasmin::Output<>, uint8_t size) {
    auto [value, location] = input;
    jasmin::Value::Store(value, location, size);
  }
};

// TODO: Remove Hack.
struct VoidConstPtr {
  VoidConstPtr(void const* ptr = nullptr) : ptr_(ptr) {}
  void const* ptr() const { return ptr_; }

  friend bool NthSerialize(auto&, VoidConstPtr) {
    NTH_UNIMPLEMENTED();
    return true;
  }

  friend bool NthDeserialize(auto&, VoidConstPtr&) {
    NTH_UNIMPLEMENTED();
    return true;
  }

 private:
  void const* ptr_;
};

struct PushStringLiteral : jasmin::Instruction<PushStringLiteral> {
  static void execute(jasmin::Input<>, jasmin::Output<char const*, size_t> out,
                      StringLiteral s) {
    std::string_view str = s.str();
    out.set(str.data(), str.size());
  }
};

struct RegisterForeignFunction : jasmin::Instruction<RegisterForeignFunction> {
  static void consume(jasmin::Input<char const*, size_t, type::Type> in,
                      jasmin::Output<void const*> out);
};

struct InvokeForeignFunction : jasmin::Instruction<InvokeForeignFunction> {
  static void consume(std::span<jasmin::Value> input,
                      std::span<jasmin::Value> output, type::FunctionType type,
                      VoidConstPtr fn_ptr);
};

struct TypeKind : jasmin::Instruction<TypeKind> {
  static void consume(jasmin::Input<type::Type> in,
                      jasmin::Output<type::Type::Kind> out) {
    out.set(in.get<0>().kind());
  }
};

struct ConstructParametersType : jasmin::Instruction<ConstructParametersType> {
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
  static void consume(std::span<jasmin::Value> inputs,
                      std::span<jasmin::Value> outputs);
};

struct ConstructRefinementType : jasmin::Instruction<ConstructRefinementType> {
  static void consume(jasmin::Input<type::Type, Pattern> in,
                      jasmin::Output<type::Type> out) {
    out.set(type::Refinement(in.get<0>(), in.get<1>()));
  }
};

struct ConstructFunctionType : jasmin::Instruction<ConstructFunctionType> {
  static void consume(jasmin::Input<type::Type, type::Type> in,
                      jasmin::Output<type::Type> out);
};

struct ConstructOpaqueType : jasmin::Instruction<ConstructOpaqueType> {
  static void consume(jasmin::Input<>, jasmin::Output<type::Type> out) {
    out.set(type::Opaque());
  }
};

struct ConstructPointerType : jasmin::Instruction<ConstructPointerType> {
  static void consume(jasmin::Input<type::Type> in,
                      jasmin::Output<type::Type> out) {
    out.set(type::Ptr(in.get<0>()));
  }
};

struct ConstructBufferPointerType
    : jasmin::Instruction<ConstructBufferPointerType> {
  static void consume(jasmin::Input<type::Type> in,
                      jasmin::Output<type::Type> out) {
    out.set(type::BufPtr(in.get<0>()));
  }
};

struct ConstructSliceType : jasmin::Instruction<ConstructSliceType> {
  static void consume(jasmin::Input<type::Type> in,
                      jasmin::Output<type::Type> out) {
    out.set(type::Slice(in.get<0>()));
  }
};

struct NoOp : jasmin::Instruction<NoOp> {
  static void execute(jasmin::Input<>, jasmin::Output<>) {}
};

struct AddPointer : jasmin::Instruction<AddPointer> {
  static void consume(jasmin::Input<std::byte const*, uint64_t> in,
                      jasmin::Output<std::byte const*> out) {
    auto [p, d] = in;
    out.set(p + d);
  }
};

struct AsciiEncode : jasmin::Instruction<AsciiEncode> {
  static void consume(jasmin::Input<uint8_t> in, jasmin::Output<char> out) {
    out.set(in.get<0>());
  }
};

struct AsciiDecode : jasmin::Instruction<AsciiDecode> {
  static void consume(jasmin::Input<char> in, jasmin::Output<uint8_t> out) {
    out.set(in.get<0>());
  }
};

struct LoadProgramArguments : jasmin::Instruction<LoadProgramArguments> {
  static void execute(jasmin::Input<> in,
                      jasmin::Output<std::byte const*, uint64_t> out);
};

struct CheckInterfaceSatisfaction
    : jasmin::Instruction<CheckInterfaceSatisfaction> {
  static void consume(jasmin::Input<type::Type>, jasmin::Output<bool>,
                      Interface);
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

template <typename... Is>
using PushInstructions = jasmin::MakeInstructionSet<jasmin::Push<Is>...>;

using InstructionSet = jasmin::MakeInstructionSet<
    PushInstructions<bool, char, std::byte, int8_t, int16_t, int32_t, int64_t,
                     uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                     Integer, type::Type, jasmin::Function<> const*,
                     type::Type::Kind, ModuleId, VoidConstPtr>,
    PushNull, PushStringLiteral, jasmin::Equal<type::Type::Kind>, Rotate,
    ConstructOpaqueType, ConstructPointerType, ConstructBufferPointerType,
    ConstructFunctionType, ConstructParametersType, ConstructSliceType,
    ConstructInterface, RegisterForeignFunction, InvokeForeignFunction,
    jasmin::Not, NoOp, Store, jasmin::Load, jasmin::StackAllocate,
    jasmin::StackOffset, jasmin::Add<int64_t>, jasmin::Subtract<int64_t>,
    jasmin::Multiply<int64_t>, jasmin::Mod<int64_t>, jasmin::Equal<int64_t>,
    jasmin::LessThan<int64_t>, AddPointer, LoadProgramArguments,
    jasmin::Duplicate, AsciiEncode, AsciiDecode, jasmin::Drop, jasmin::Swap,
    TypeKind, jasmin::Negate<int8_t>, jasmin::Negate<int16_t>,
    jasmin::Negate<int32_t>, jasmin::Negate<int64_t>, jasmin::Negate<Integer>,
    jasmin::Negate<float>, jasmin::Negate<double>, ConstructRefinementType,
    CheckInterfaceSatisfaction>;

using IrFunction      = jasmin::Function<InstructionSet>;
using ProgramFragment = jasmin::ProgramFragment<InstructionSet>;

void Extend(Interface intf, type::Type t);

struct SharedContext {
  jasmin::FunctionRegistry registry;
  ProgramFragment foreign;
};
inline SharedContext shared_context;

}  // namespace ic

#endif  // ICARUS_IR_FUNCTION_H
