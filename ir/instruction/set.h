#ifndef ICARUS_IR_INSTRUCTION_SET_H
#define ICARUS_IR_INSTRUCTION_SET_H

#include <array>
#include <optional>
#include <type_traits>
#include <utility>
#include <vector>

#include "absl/random/random.h"
#include "absl/strings/str_format.h"
#include "base/meta.h"
#include "ir/compiled_jump.h"
#include "ir/instruction/core.h"
#include "ir/read_only_data.h"
#include "ir/value/addr.h"
#include "ir/value/block.h"
#include "ir/value/fn.h"
#include "ir/value/foreign_fn.h"
#include "ir/value/native_fn.h"
#include "type/primitive.h"

namespace ir {

template <typename... InstructionsOrSets>
struct InstructionSet;

namespace internal_instructions {

template <typename... Processed>
auto ExpandedInstructions(base::type_list<>, base::type_list<Processed...>) {
  return base::type_list<Processed...>{};
}

template <typename T, typename... Ts, typename... Processed>
auto ExpandedInstructions(base::type_list<T, Ts...>,
                          base::type_list<Processed...>) {
  if constexpr (((base::meta<T> == base::meta<Processed>) or ...)) {
    return ExpandedInstructions(base::type_list<Ts...>{},
                                base::type_list<Processed...>{});
  } else if constexpr (base::meta<T>.template is_a<InstructionSet>()) {
    return ExpandedInstructions(base::type_list_cat<typename T::instructions_t,
                                                    base::type_list<Ts...>>{},
                                base::type_list<Processed...>{});
  } else {
    return ExpandedInstructions(base::type_list<Ts...>{},
                                base::type_list<T, Processed...>{});
  }
}

template <typename... Insts>
absl::flat_hash_map<base::MetaValue, cmd_index_t> MakeInstructionIndexMapping(
    base::type_list<Insts...>) {
  cmd_index_t index = 0;
  return {{base::meta<LoadInstruction>, LoadInstruction::kIndex},
          {base::meta<Insts>, index++}...};
}

}  // namespace internal_instructions

template <typename... InstructionsOrSets>
struct InstructionSet {
  using instructions_t = decltype(internal_instructions::ExpandedInstructions(
      base::type_list<InstructionsOrSets...>{}, base::type_list<>{}));

  static cmd_index_t Index(Inst const &inst) {
    auto iter = index_mapping_.find(inst.rtti());
#if defined(ICARUS_DEBUG)
    if (iter == index_mapping_.end()) {
      LOG("", "Failed to find instruction %s", inst.rtti().name());
    }
#endif
    return iter->second;
  }

 private:
  static absl::flat_hash_map<base::MetaValue, cmd_index_t> const index_mapping_;
};

template <typename... InstructionsOrSets>
absl::flat_hash_map<base::MetaValue, cmd_index_t> const
    InstructionSet<InstructionsOrSets...>::index_mapping_ =
        internal_instructions::MakeInstructionIndexMapping(
            static_cast<instructions_t>(nullptr));

template <typename... Ts>
using CoreInstructionsImpl = InstructionSet<
    RegisterInstruction<Ts>..., StoreInstruction<Ts>..., PhiInstruction<Ts>...,
    SetReturnInstruction<Ts>..., GetReturnInstruction, CallInstruction,
    SetReturnInstruction<core::Bytes>, SetReturnInstruction<core::Alignment>,
    SetReturnInstruction<GenericFn>,
    SetReturnInstruction<type::GenericStruct const *>>;
using CoreInstructions =
    CoreInstructionsImpl<bool, uint8_t, int8_t, uint16_t, int16_t, uint32_t,
                         int32_t, uint64_t, int64_t, float, double, FlagsVal,
                         type::Type const *, Addr, EnumVal, String, Fn, Block,
                         Scope, Jump, ModuleId>;

}  // namespace ir

#endif  //  ICARUS_IR_INSTRUCTION_SET_H
