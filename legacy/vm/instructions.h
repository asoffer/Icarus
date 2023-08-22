#ifndef ICARUS_VM_INSTRUCTIONS_H
#define ICARUS_VM_INSTRUCTIONS_H

#include <cstdint>
#include <type_traits>

#include "core/integer.h"
#include "jasmin/debug.h"
#include "jasmin/function.h"
#include "jasmin/instruction.h"
#include "jasmin/instructions/arithmetic.h"
#include "jasmin/instructions/bool.h"
#include "jasmin/instructions/compare.h"
#include "jasmin/instructions/core.h"
#include "jasmin/instructions/stack.h"
#include "jasmin/serialization.h"
#include "jasmin/value_stack.h"
#include "module/global_function_map.h"
#include "module/unique_id.h"
#include "semantic_analysis/type_system.h"
#include "serialization/foreign_symbol_map.h"
#include "serialization/read_only_data.h"
#include "vm/argument_slice.h"
#include "vm/function_table.h"
#include "vm/immediate_values.h"
#include "vm/instructions.h"

namespace vm {

template <bool FromSigned, bool ToSigned>
struct ZeroExtend
    : jasmin::StackMachineInstruction<ZeroExtend<FromSigned, ToSigned>> {
  using Options = ZeroExtendOptions;

  static void execute(jasmin::ValueStack& value_stack, Options options) {
    std::conditional_t<FromSigned, int64_t, uint64_t> scratch;
    if (options.from_bits <= 8) {
      using from_type = std::conditional_t<FromSigned, int8_t, uint8_t>;
      scratch         = value_stack.pop<from_type>();
    } else if (options.from_bits <= 16) {
      using from_type = std::conditional_t<FromSigned, int16_t, uint16_t>;
      scratch         = value_stack.pop<from_type>();
    } else if (options.from_bits <= 32) {
      using from_type = std::conditional_t<FromSigned, int32_t, uint32_t>;
      scratch         = value_stack.pop<from_type>();
    } else {
      using from_type = std::conditional_t<FromSigned, int64_t, uint64_t>;
      scratch         = value_stack.pop<from_type>();
    }

    if (options.to_bits <= 8) {
      using to_type = std::conditional_t<ToSigned, int8_t, uint8_t>;
      value_stack.push(static_cast<to_type>(scratch));
    } else if (options.to_bits <= 16) {
      using to_type = std::conditional_t<ToSigned, int16_t, uint16_t>;
      value_stack.push(static_cast<to_type>(scratch));
    } else if (options.to_bits <= 32) {
      using to_type = std::conditional_t<ToSigned, int32_t, uint32_t>;
      value_stack.push(static_cast<to_type>(scratch));
    } else {
      using to_type = std::conditional_t<ToSigned, int64_t, uint64_t>;
      value_stack.push(static_cast<to_type>(scratch));
    }
  }
};

struct BuiltinForeignFunction
    : jasmin::StackMachineInstruction<BuiltinForeignFunction> {
  static void execute(jasmin::ValueStack& value_stack, core::Type t,
                      FunctionTable* table,
                      serialization::ForeignSymbolMap* foreign_symbol_map,
                      semantic_analysis::TypeSystem* ts);
};

struct BuiltinForeignPointer
    : jasmin::StackMachineInstruction<BuiltinForeignPointer> {
  using execution_state = serialization::ForeignSymbolMap;
  static void execute(jasmin::ValueStack& value_stack,
                      execution_state& foreign_symbol_map, core::Type type);
};

struct TranslateFunctionArguments
    : jasmin::StackMachineInstruction<TranslateFunctionArguments> {
  static void execute(jasmin::ValueStack& value_stack,
                      core::Parameters<core::Type> const* parameters,
                      module::UniqueId module_id);
};

struct InvokeForeignFunction
    : jasmin::StackMachineInstruction<InvokeForeignFunction> {
  using serialization_state = serialization::ForeignSymbolMap;

  static void serialize(jasmin::Serializer& serializer,
                        std::span<jasmin::Value const> values,
                        serialization_state& state);
  static bool deserialize(jasmin::Deserializer& deserializer,
                          std::span<jasmin::Value> values,
                          serialization_state& state);

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
  using function_state = TemporarySpace;
  static void execute(jasmin::ValueStack& value_stack, function_state& space,
                      size_t size_in_bytes) {
    auto* p = space.allocate(size_in_bytes);
    value_stack.push(p);
  }
};

struct PushStringLiteral : jasmin::StackMachineInstruction<PushStringLiteral> {
  using serialization_state = serialization::ReadOnlyData;
  static constexpr void execute(jasmin::ValueStack& value_stack,
                                char const* ptr, size_t length) {
    value_stack.push(ptr);
    value_stack.push(length);
  }

  static void serialize(jasmin::Serializer& serializer,
                        std::span<jasmin::Value const> values,
                        serialization_state& state);
  static bool deserialize(jasmin::Deserializer& deserializer,
                          std::span<jasmin::Value> values,
                          serialization_state& state);

  static std::string debug(std::span<jasmin::Value const, 2> immediates);
};

struct PushFunction : jasmin::StackMachineInstruction<PushFunction> {
  using serialization_state =
      std::tuple<module::UniqueId, module::GlobalFunctionMap&>;

  static void execute(jasmin::ValueStack& value_stack, jasmin::Value value);

  static void serialize(jasmin::Serializer& serializer,
                        std::span<jasmin::Value const> values,
                        serialization_state& state);
  static bool deserialize(jasmin::Deserializer& deserializer,
                          std::span<jasmin::Value> values,
                          serialization_state& state);

  static std::string debug(std::span<jasmin::Value const, 1> immediates);
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
  using function_state = TemporarySpace;
  static void execute(function_state& space) { space.free_all(); }
};


struct IncrementPointerDynamic : jasmin::StackMachineInstruction<IncrementPointerDynamic> {
  static void* execute(void* ptr, uint64_t amount) {
    return static_cast<char*>(ptr) + amount;
  }

  static std::string debug() { return "increment-pointer-dynamic"; }
};

struct IncrementPointer : jasmin::StackMachineInstruction<IncrementPointer> {
  static void execute(jasmin::ValueStack& value_stack, size_t amount) {
    value_stack.push(value_stack.pop<char*>() + amount);
  }

  static std::string debug(std::span<jasmin::Value const, 1> immediates) {
    return "increment-pointer " + std::to_string(immediates[0].as<size_t>()) +
           " byte(s)";
  }
};

template <template <typename> typename I, typename... Ts>
using ApplyInstruction = jasmin::MakeInstructionSet<I<Ts>...>;

struct BuiltinAsciiDecode
    : jasmin::StackMachineInstruction<BuiltinAsciiDecode> {
  static uint8_t execute(data_types::Char c) { return c.as_type<uint8_t>(); }

  static std::string debug() { return "builtin.ascii-decode"; }
};

struct BuiltinAsciiEncode
    : jasmin::StackMachineInstruction<BuiltinAsciiEncode> {
  static data_types::Char execute(uint8_t n) { return data_types::Char(n); }

  static std::string debug() { return "builtin.ascii-encode"; }
};

struct BuiltinArguments : jasmin::StackMachineInstruction<BuiltinArguments> {
  using execution_state = ArgumentSlice;
  static void execute(jasmin::ValueStack& value_stack, execution_state& state) {
    value_stack.push(state.data());
    value_stack.push(static_cast<uint64_t>(state.length()));
  }

  static std::string debug() { return "builtin.arguments"; }
};

struct BuiltinOpaque : jasmin::StackMachineInstruction<BuiltinOpaque> {
  using execution_state = semantic_analysis::TypeSystem;
  static void execute(jasmin::ValueStack& value_stack, execution_state& state) {
    core::Type t = semantic_analysis::OpaqueType(state);
    value_stack.push(t);
  }

  static std::string debug() { return "builtin.ascii-encode"; }
};

using BuiltinInstructionSet =
    jasmin::MakeInstructionSet<BuiltinAsciiDecode, BuiltinAsciiEncode,
                               BuiltinOpaque, BuiltinArguments>;

// TODO: core::*Type instructions should be registerable and not required to
// be explicitly added here.
struct InstructionSet
    : jasmin::MakeInstructionSet<
          jasmin::Push, jasmin::DuplicateAt, jasmin::Duplicate, jasmin::Swap,
          jasmin::Not, jasmin::Drop, jasmin::Store, jasmin::Xor,
          semantic_analysis::TypeSystem::JasminInstructionSet,
          core::ParameterType::Begin, core::ParameterType::Append,
          core::ParameterType::AppendNamed,
          core::ParameterType::End<semantic_analysis::TypeSystem>,
          core::FunctionType::End<semantic_analysis::TypeSystem>,
          jasmin::StackAllocate, jasmin::StackOffset, jasmin::Load,
          AllocateTemporary, DeallocateAllTemporaries, BuiltinForeignFunction,
          BuiltinForeignPointer, InvokeForeignFunction, PushStringLiteral,
          PushFunction, IncrementPointer, IncrementPointerDynamic,
          TranslateFunctionArguments, vm::ZeroExtend<true, true>,
          vm::ZeroExtend<false, true>, vm::ZeroExtend<false, false>,
          ApplyInstruction<jasmin::Equal, int8_t, int16_t, int32_t, int64_t,
                           uint8_t, uint16_t, uint32_t, uint64_t, float,
                           double>,
          ApplyInstruction<jasmin::LessThan, int8_t, int16_t, int32_t, int64_t,
                           uint8_t, uint16_t, uint32_t, uint64_t, float,
                           double>,
          ApplyInstruction<jasmin::AppendEqual, int8_t, int16_t, int32_t,
                           int64_t, uint8_t, uint16_t, uint32_t, uint64_t,
                           float, double>,
          ApplyInstruction<jasmin::AppendLessThan, int8_t, int16_t, int32_t,
                           int64_t, uint8_t, uint16_t, uint32_t, uint64_t,
                           float, double>,
          ApplyInstruction<Construct, bool, data_types::Char, int8_t, int16_t,
                           int32_t, int64_t, uint8_t, uint16_t, uint32_t,
                           uint64_t, float, double, core::Integer,
                           data_types::addr_t>,
          ApplyInstruction<jasmin::Add, int8_t, int16_t, int32_t, int64_t,
                           uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                           core::Integer>,
          ApplyInstruction<jasmin::Subtract, int8_t, int16_t, int32_t, int64_t,
                           uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                           core::Integer>,
          ApplyInstruction<jasmin::Multiply, int8_t, int16_t, int32_t, int64_t,
                           uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                           core::Integer>,
          ApplyInstruction<jasmin::Divide, int8_t, int16_t, int32_t, int64_t,
                           uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                           core::Integer>,
          ApplyInstruction<jasmin::Mod, int8_t, int16_t, int32_t, int64_t,
                           uint8_t, uint16_t, uint32_t, uint64_t,
                           core::Integer>,
          ApplyInstruction<jasmin::Negate, int8_t, int16_t, int32_t, int64_t,
                           uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                           core::Integer>,
          jasmin::DumpValueStack, BuiltinInstructionSet> {};

}  // namespace vm

#endif  // ICARUS_VM_INSTRUCTIONS_H