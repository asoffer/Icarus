#ifndef ICARUS_SEMANTIC_ANALYSIS_INSTRUCTION_SET_H
#define ICARUS_SEMANTIC_ANALYSIS_INSTRUCTION_SET_H

#include "ir/value/integer.h"
#include "jasmin/function.h"
#include "jasmin/instructions/arithmetic.h"
#include "jasmin/instructions/core.h"
#include "jasmin/instructions/stack.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {
// Defined in "semantic_analysis/foreign_function_map.h", in the same build
// target.
struct ForeignFunctionMap;

struct BuiltinForeign : jasmin::StackMachineInstruction<BuiltinForeign> {
  static void execute(jasmin::ValueStack& value_stack, core::Type t,
                      ForeignFunctionMap* module);
};

struct InvokeForeignFunction
    : jasmin::StackMachineInstruction<InvokeForeignFunction> {
  static void execute(jasmin::ValueStack& value_stack, void (*fn_ptr)(),
                      core::Parameter<core::Type> const* parameters,
                      size_t parameter_count,
                      core::Type const* maybe_return_type);
};

struct TemporarySpace {
  // TODO: It is particularly inefficient to separately allocate each of these.
  std::byte* allocate(size_t size_in_bytes) {
    return allocations_.emplace_back(new std::byte[size_in_bytes]).get();
  }

  void free_all() { allocations_.clear(); }

 private:
  std::vector<std::unique_ptr<std::byte[]>> allocations_;
};

struct AllocateTemporary : jasmin::StackMachineInstruction<AllocateTemporary> {
  using JasminFunctionState = TemporarySpace;
  static void execute(jasmin::ValueStack& value_stack,
                      JasminFunctionState& space, size_t size_in_bytes) {
    auto* p = space.allocate(size_in_bytes);
    value_stack.push(p);
  }
};

template <typename T>
struct Construct : jasmin::StackMachineInstruction<Construct<T>> {
  static void execute(jasmin::ValueStack& value_stack,
                      T value) requires(jasmin::SmallTrivialValue<T>) {
    value_stack.push(new (value_stack.pop<std::byte*>()) T(value));
  }
  static void execute(jasmin::ValueStack& value_stack, T const* value) requires(
      not jasmin::SmallTrivialValue<T>) {
    value_stack.push(new (value_stack.pop<std::byte*>()) T(*value));
  }
};

template <typename T>
struct CopyConstruct : jasmin::StackMachineInstruction<CopyConstruct<T>> {
  static T* execute(T const* from, T* to) { return new (to) T(*from); }
};

template <typename T>
struct MoveConstruct : jasmin::StackMachineInstruction<MoveConstruct<T>> {
  static T* execute(T const* from, T* to) {
    return new (to) T(std::move(*from));
  }
};

template <typename T>
struct Destroy : jasmin::StackMachineInstruction<Destroy<T>> {
  static void execute(jasmin::ValueStack& value_stack) {
    value_stack.pop<T*>()->~T();
  }
};

struct DeallocateAllTemporaries
    : jasmin::StackMachineInstruction<DeallocateAllTemporaries> {
  using JasminFunctionState = TemporarySpace;
  static void execute(JasminFunctionState& space) { space.free_all(); }
};

struct NegateInteger : jasmin::StackMachineInstruction<NegateInteger> {
  static void execute(jasmin::ValueStack& value_stack) {
    auto& value = *value_stack.pop<ir::Integer*>();
    value       = -value;
    value_stack.push(&value);
  }
};

namespace internal_byte_code {

template <template <typename> typename I, typename... Ts>
using ApplyInstruction = jasmin::MakeInstructionSet<I<Ts>...>;

// TOOD: core::*Type instructions should be registerable and not required to
// be explicitly added here.
using InstructionSet = jasmin::MakeInstructionSet<
    jasmin::Push, jasmin::DuplicateAt, jasmin::Store,
    TypeSystem::JasminInstructionSet, core::ParameterType::Begin,
    core::ParameterType::Append, core::ParameterType::AppendNamed,
    core::ParameterType::End<TypeSystem>, core::FunctionType::End<TypeSystem>,
    jasmin::StackAllocate, jasmin::StackOffset, jasmin::Load, AllocateTemporary,
    DeallocateAllTemporaries, BuiltinForeign, InvokeForeignFunction,
    ApplyInstruction<Construct, bool, ir::Char, int8_t, int16_t, int32_t,
                     int64_t, ir::Integer, uint8_t, uint16_t, uint32_t,
                     uint64_t, float, double>,
    Destroy<ir::Integer>, CopyConstruct<ir::Integer>,
    MoveConstruct<ir::Integer>, NegateInteger,
    ApplyInstruction<jasmin::Negate, int8_t, int16_t, int32_t, int64_t, uint8_t,
                     uint16_t, uint32_t, uint64_t, float, double>>;

}  // namespace internal_byte_code

using IrFunction = jasmin::Function<internal_byte_code::InstructionSet>;

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_INSTRUCTION_SET_H
