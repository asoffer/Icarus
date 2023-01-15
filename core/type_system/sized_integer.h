#ifndef ICARUS_CORE_TYPE_SYSTEM_SIZED_INTEGER_H
#define ICARUS_CORE_TYPE_SYSTEM_SIZED_INTEGER_H

#include "core/alignment.h"
#include "core/bytes.h"
#include "core/type_system/type_system.h"

namespace core {
namespace internal_sized_integer {

struct SizedIntegerTypeState {
  using store_inline = void;

  uint16_t size_in_bits : 10;
  // Alignment must always be a power of 2, so we store its base-2 logarithm.
  uint16_t log_alignment_in_bytes : 5;
  uint16_t is_signed : 1;

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

constexpr uint8_t SmallestPowerOfTwoGreaterThanOrEqualTo(uint32_t n) {
  --n;
  n |= (n >> 1);
  n |= (n >> 2);
  n |= (n >> 4);
  n |= (n >> 8);
  n |= (n >> 16);
  return n + 1;
}

}  // namespace internal_sized_integer

// Represents types whose storage is sufficient to hold values in the range
// -2^{N-1} ... 2^{N-1}-1 (inclusive) for signed types and 0 ... 2^N - 1
// (inclusive) for unsigned types.
struct SizedIntegerType
    : TypeCategory<SizedIntegerType,
                   internal_sized_integer::SizedIntegerTypeState> {
  template <TypeSystemSupporting<SizedIntegerType> TS>
  static SizedIntegerType I(uint16_t bits) {
    return I<TS>(bits, DefaultAlignment(bits));
  }
  template <TypeSystemSupporting<SizedIntegerType> TS>
  static SizedIntegerType I(uint16_t bits, Alignment alignment) {
    size_t log_alignment = internal_sized_integer::Log2(alignment.value());
    ASSERT(log_alignment <
           static_cast<size_t>(std::numeric_limits<uint16_t>::max()));
    return SizedIntegerType(
        nth::type<TS>,
        {.size_in_bits           = bits,
         .log_alignment_in_bytes = static_cast<uint16_t>(log_alignment),
         .is_signed              = true});
  }
  template <TypeSystemSupporting<SizedIntegerType> TS>
  static SizedIntegerType U(uint16_t bits) {
    return U<TS>(bits, DefaultAlignment(bits));
  }
  template <TypeSystemSupporting<SizedIntegerType> TS>
  static SizedIntegerType U(uint16_t bits, Alignment alignment) {
    size_t log_alignment = internal_sized_integer::Log2(alignment.value());
    ASSERT(log_alignment <
           static_cast<size_t>(std::numeric_limits<uint16_t>::max()));
    return SizedIntegerType(
        nth::type<TS>,
        {.size_in_bits           = bits,
         .log_alignment_in_bytes = static_cast<uint16_t>(log_alignment),
         .is_signed              = false});
  }

  constexpr bool is_signed() const { return value().is_signed; }
  constexpr size_t bits() const { return value().size_in_bits; }
  constexpr Bytes bytes() const {
    return Bytes((value().size_in_bits + 7) / 8);
  }
  constexpr Alignment alignment() const {
    return Alignment(uint64_t{1} << value().log_alignment_in_bytes);
  }

  static Alignment DefaultAlignment(size_t bits) {
    return Alignment(
        internal_sized_integer::SmallestPowerOfTwoGreaterThanOrEqualTo(
            (bits + 7) / 8));
  }

 private:
  friend TypeCategory;
  template <typename...>
  friend struct TypeSystem;

  constexpr internal_sized_integer::SizedIntegerTypeState const& value() const {
    return std::get<0>(decompose());
  }

  explicit constexpr SizedIntegerType(
      nth::Type auto t,
      internal_sized_integer::SizedIntegerTypeState state) requires
      TypeSystemSupporting<typename decltype(t)::type, SizedIntegerType>
      : TypeCategory(t, state) {}
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_SIZED_INTEGER_H
