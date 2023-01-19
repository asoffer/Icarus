#ifndef ICARUS_SEMANTIC_ANALYSIS_INSTRUCTION_SET_H
#define ICARUS_SEMANTIC_ANALYSIS_INSTRUCTION_SET_H

#include "jasmin/function.h"
#include "jasmin/instructions/arithmetic.h"
#include "jasmin/instructions/bool.h"
#include "jasmin/instructions/compare.h"
#include "jasmin/instructions/core.h"
#include "jasmin/instructions/stack.h"
#include "jasmin/serialization.h"
#include "module/data/integer_table.h"
#include "nth/container/flyweight_set.h"
#include "nth/numeric/integer.h"
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
  struct serialization_state {
    void set_foreign_function_map(
        ForeignFunctionMap const* foreign_function_map) {
      foreign_function_map_ = foreign_function_map;
    }

    void set_map(
        absl::flat_hash_map<void (*)(), std::pair<size_t, core::Type>> map) {
      map_ = std::move(map);
    }

    std::pair<size_t, core::Type> const& operator[](void (*fn_ptr)()) const {
      auto iter = map_.find(fn_ptr);
      ASSERT(iter != map_.end());
      return iter->second;
    }

    std::type_identity_t<void (*)()> FunctionPointer(size_t index) const;

    TypeSystem& type_system() const;

   private:
    ForeignFunctionMap const* foreign_function_map_;
    absl::flat_hash_map<void (*)(), std::pair<size_t, core::Type>> map_;
  };

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
  using JasminFunctionState = TemporarySpace;
  static void execute(jasmin::ValueStack& value_stack,
                      JasminFunctionState& space, size_t size_in_bytes) {
    auto* p = space.allocate(size_in_bytes);
    value_stack.push(p);
  }
};

struct PushStringLiteral : jasmin::StackMachineInstruction<PushStringLiteral> {
  struct serialization_state {
    size_t index(std::string_view s) {
      return strings_.index(strings_.insert(s).first);
    }

    std::string_view string(size_t n) { return strings_.from_index(n); }
    size_t size() const { return strings_.size(); }
    auto begin() const { return strings_.begin(); }
    auto end() const { return strings_.end(); }

   private:
    nth::flyweight_set<std::string_view> strings_;
  };
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
};

struct PushFunction : jasmin::StackMachineInstruction<PushFunction> {
  struct serialization_state;

  static constexpr void execute(jasmin::ValueStack& value_stack,
                                jasmin::Value value) {
    value_stack.push(value);
  }

  static void serialize(jasmin::Serializer& serializer,
                        std::span<jasmin::Value const> values,
                        serialization_state& state);
  static bool deserialize(jasmin::Deserializer& deserializer,
                          std::span<jasmin::Value> values,
                          serialization_state& state);
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
  using JasminExecutionState = module::IntegerTable;
  static nth::Integer const* execute(JasminExecutionState& table,
                                     nth::Integer const* n) {
    return table.insert(-*n);
  }
};

namespace internal_byte_code {

template <template <typename> typename I, typename... Ts>
using ApplyInstruction = jasmin::MakeInstructionSet<I<Ts>...>;

// TOOD: core::*Type instructions should be registerable and not required to
// be explicitly added here.
using InstructionSet = jasmin::MakeInstructionSet<
    jasmin::Push, jasmin::DuplicateAt, jasmin::Duplicate, jasmin::Swap,
    jasmin::Not, jasmin::Drop, jasmin::Store, jasmin::Xor,
    TypeSystem::JasminInstructionSet, core::ParameterType::Begin,
    core::ParameterType::Append, core::ParameterType::AppendNamed,
    core::ParameterType::End<TypeSystem>, core::FunctionType::End<TypeSystem>,
    jasmin::StackAllocate, jasmin::StackOffset, jasmin::Load, AllocateTemporary,
    DeallocateAllTemporaries, BuiltinForeign, InvokeForeignFunction,
    PushStringLiteral, PushFunction,
    ApplyInstruction<jasmin::Equal, int8_t, int16_t, int32_t, int64_t, uint8_t,
                     uint16_t, uint32_t, uint64_t, float, double>,
    ApplyInstruction<jasmin::LessThan, int8_t, int16_t, int32_t, int64_t,
                     uint8_t, uint16_t, uint32_t, uint64_t, float, double>,
    ApplyInstruction<jasmin::AppendEqual, int8_t, int16_t, int32_t, int64_t,
                     uint8_t, uint16_t, uint32_t, uint64_t, float, double>,
    ApplyInstruction<jasmin::AppendLessThan, int8_t, int16_t, int32_t, int64_t,
                     uint8_t, uint16_t, uint32_t, uint64_t, float, double>,
    ApplyInstruction<Construct, bool, ir::Char, int8_t, int16_t, int32_t,
                     int64_t, nth::Integer, uint8_t, uint16_t, uint32_t,
                     uint64_t, float, double>,
    Destroy<nth::Integer>, CopyConstruct<nth::Integer>,
    MoveConstruct<nth::Integer>, NegateInteger,
    ApplyInstruction<jasmin::Add, int8_t, int16_t, int32_t, int64_t, uint8_t,
                     uint16_t, uint32_t, uint64_t, float, double>,
    ApplyInstruction<jasmin::Subtract, int8_t, int16_t, int32_t, int64_t,
                     uint8_t, uint16_t, uint32_t, uint64_t, float, double>,
    ApplyInstruction<jasmin::Multiply, int8_t, int16_t, int32_t, int64_t,
                     uint8_t, uint16_t, uint32_t, uint64_t, float, double>,
    ApplyInstruction<jasmin::Divide, int8_t, int16_t, int32_t, int64_t, uint8_t,
                     uint16_t, uint32_t, uint64_t, float, double>,
    ApplyInstruction<jasmin::Mod, int8_t, int16_t, int32_t, int64_t, uint8_t,
                     uint16_t, uint32_t, uint64_t>,
    ApplyInstruction<jasmin::Negate, int8_t, int16_t, int32_t, int64_t, uint8_t,
                     uint16_t, uint32_t, uint64_t, float, double>>;

}  // namespace internal_byte_code

using internal_byte_code::InstructionSet;  // TODO: Make public.
using IrFunction = jasmin::Function<internal_byte_code::InstructionSet>;

struct PushFunction::serialization_state {
  size_t index(IrFunction const* s) {
    return functions_.index(functions_.insert(s).first);
  }

  IrFunction const* function(size_t n) { return functions_.from_index(n); }

  size_t size() const { return functions_.size(); }
  auto begin() const { return functions_.begin(); }
  auto end() const { return functions_.end(); }

 private:
  nth::flyweight_set<IrFunction const*> functions_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_INSTRUCTION_SET_H
