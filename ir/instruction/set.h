#ifndef ICARUS_IR_INSTRUCTION_SET_H
#define ICARUS_IR_INSTRUCTION_SET_H

#include <array>
#include <type_traits>

#include "base/meta.h"
#include "ir/interpretter/instructions.h"

namespace ir {

template <typename Capabilities, typename... InstructionsOrSets>
struct InstructionSet;

namespace internal_instructions {

template <typename>
struct IsInstructionSet : std::false_type {};

template <typename Capabilities, typename... InstructionsOrSets>
struct IsInstructionSet<InstructionSet<Capabilities, InstructionsOrSets...>>
    : std::true_type {};

template <typename... Processed>
auto ExpandedInstructions(base::type_list<>, base::type_list<Processed...>) {
  return static_cast<base::type_list<Processed...>>(nullptr);
}

template <typename T, typename... Ts, typename... Processed>
auto ExpandedInstructions(base::type_list<T, Ts...>,
                          base::type_list<Processed...>) {
  if constexpr (((base::meta<T> == base::meta<Processed>) or ...)) {
    return ExpandedInstructions(
        static_cast<base::type_list<Ts...>>(nullptr),
        static_cast<base::type_list<Processed...>>(nullptr));
  } else if constexpr (IsInstructionSet<T>::value) {
    return ExpandedInstructions(
        static_cast<base::type_list_cat<typename T::instructions,
                                        base::type_list<Ts...>>>(nullptr),
        static_cast<base::type_list<Processed...>>(nullptr));
  } else {
    return ExpandedInstructions(
        static_cast<base::type_list<Ts...>>(nullptr),
        static_cast<base::type_list<T, Processed...>>(nullptr));
  }
}

using exec_t = void (*)(base::untyped_buffer::const_iterator *,
                        interpretter::ExecutionContext *,
                        absl::Span<ir::Addr const>);

template <typename... Insts>
absl::flat_hash_map<int, exec_t> MakeExecuteFunctions(
    base::type_list<Insts...>) {
  return {{Insts::kIndex, interpretter::kInstructions[Insts::kIndex]}...};
}

}  // namespace internal_instructions

struct RequiredCapabilities;

template <typename Capabilities, typename... InstructionsOrSets>
struct InstructionSet;

template <typename... Capabilities, typename... InstructionsOrSets>
struct InstructionSet<RequiredCapabilities(Capabilities...),
                      InstructionsOrSets...> {
  using instructions_t = decltype(internal_instructions::ExpandedInstructions(
      static_cast<base::type_list<InstructionsOrSets...>>(nullptr),
      static_cast<base::type_list<>>(nullptr)));
  static absl::flat_hash_map<int, internal_instructions::exec_t> Execute;

};

template <typename... Capabilities, typename... InstructionsOrSets>
absl::flat_hash_map<int, internal_instructions::exec_t> InstructionSet<
    RequiredCapabilities(Capabilities...), InstructionsOrSets...>::Execute =
    internal_instructions::MakeExecuteFunctions(
        static_cast<instructions_t>(nullptr));

}  // namespace ir

#endif  //  ICARUS_IR_INSTRUCTION_SET_H
