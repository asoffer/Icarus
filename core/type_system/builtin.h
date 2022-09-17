#ifndef ICARUS_CORE_TYPE_SYSTEM_BUILTIN_H
#define ICARUS_CORE_TYPE_SYSTEM_BUILTIN_H

#include "core/type_system/type_system.h"

namespace core {
namespace internal_type_system_builtin {

struct BuiltinTypeState {
  uint16_t size_in_bits : 10;
  uint16_t alignment_in_bytes : 6;
  enum class Kind : uint16_t {
    Bool,
    Char,
    Byte,
    Signed,
    Unsigned,
    FloatingPoint
  };
  Kind kind;

  template <typename H>
  friend H AbslHashValue(H h, BuiltinTypeState state) {
    return H::combine(std::move(h), state.size_in_bits,
                      state.alignment_in_bytes, state.kind);
  }
  bool operator==(BuiltinTypeState const&) const = default;
  bool operator!=(BuiltinTypeState const&) const = default;
};
}  // namespace internal_type_system_builtin

struct BuiltinType
    : TypeCategory<BuiltinType,
                   internal_type_system_builtin::BuiltinTypeState> {
  template <uint32_t Bits, uint32_t AlignmentBytes = (Bits + 7) / 8>
  static BuiltinType I(TypeSystemSupporting<BuiltinType> auto& s) {
    return BuiltinType(
        s,
        {.size_in_bits       = Bits,
         .alignment_in_bytes = AlignmentBytes,
         .kind = internal_type_system_builtin::BuiltinTypeState::Kind::Signed});
  }

  template <uint32_t Bits, uint32_t AlignmentBytes = (Bits + 7) / 8>
  static BuiltinType U(TypeSystemSupporting<BuiltinType> auto& s) {
    return BuiltinType(
        s,
        {.size_in_bits       = Bits,
         .alignment_in_bytes = AlignmentBytes,
         .kind =
             internal_type_system_builtin::BuiltinTypeState::Kind::Unsigned});
  }

 private:
  friend TypeCategory;

  explicit BuiltinType(TypeSystemSupporting<BuiltinType> auto& s,
                       internal_type_system_builtin::BuiltinTypeState t)
      : TypeCategory(s, t) {}
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_BUILTIN_H
