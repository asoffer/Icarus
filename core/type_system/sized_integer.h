#ifndef ICARUS_CORE_TYPE_SYSTEM_SIZED_INTEGER_H
#define ICARUS_CORE_TYPE_SYSTEM_SIZED_INTEGER_H

#include "core/type_system/type_system.h"

namespace core {
namespace internal_sized_integer {

struct SizedIntegerTypeState {
  uint16_t size_in_bits : 10;
  // Alignment must always be a power of 2, so we store its base-2 logarithm.
  uint16_t log_alignment_in_bytes: 5;
  uint16_t is_signed: 1;

  template <typename H>
  friend H AbslHashValue(H h, SizedIntegerTypeState state) {
    return H::combine(std::move(h), state.size_in_bits,
                      state.log_alignment_in_bytes, state.is_signed);
  }
  friend bool operator!=(SizedIntegerTypeState,
                         SizedIntegerTypeState) = default;

  friend bool operator==(SizedIntegerTypeState,
                         SizedIntegerTypeState) = default;
};

constexpr uint8_t Log2(uint32_t n) {
  uint8_t result = 0;
  if (n & uint32_t{0b11111111'11111111'00000000'00000000}) { result += 16; }
  if (n & uint32_t{0b11111111'00000000'11111111'00000000}) { result += 8; }
  if (n & uint32_t{0b11110000'11110000'11110000'11110000}) { result += 4; }
  if (n & uint32_t{0b11001100'11001100'11001100'11001100}) { result += 2; }
  if (n & uint32_t{0b10101010'10101010'10101010'10101010}) { result += 1; }
  return result;
}

}  // namespace internal_sized_integer

// Represents types whose storage is sufficient to hold values in the range
// -2^{N-1} ... 2^{N-1}-1 (inclusive) for signed types and 0 ... 2^N - 1
// (inclusive) for unsigned types.
struct SizedIntegerType
    : TypeCategory<SizedIntegerType,
                   internal_sized_integer::SizedIntegerTypeState> {
  template <uint32_t Bits, uint32_t AlignmentBytes = (Bits + 7) / 8>
  static SizedIntegerType I(TypeSystemSupporting<SizedIntegerType> auto& s) {
    static constexpr size_t kLogAlignment =
        internal_sized_integer::Log2(AlignmentBytes);
    return SizedIntegerType(s, {.size_in_bits           = Bits,
                                .log_alignment_in_bytes = kLogAlignment,
                                .is_signed              = true});
  }

  template <uint32_t Bits, uint32_t AlignmentBytes = (Bits + 7) / 8>
  static SizedIntegerType U(TypeSystemSupporting<SizedIntegerType> auto& s) {
    static constexpr size_t kLogAlignment =
        internal_sized_integer::Log2(AlignmentBytes);
    return SizedIntegerType(s, {.size_in_bits           = Bits,
                                .log_alignment_in_bytes = kLogAlignment,
                                .is_signed              = false});
  }

 private:
  friend TypeCategory;

  explicit SizedIntegerType(TypeSystemSupporting<SizedIntegerType> auto& s,
                            internal_sized_integer::SizedIntegerTypeState t)
      : TypeCategory(s, t) {}
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_SIZED_INTEGER_H
