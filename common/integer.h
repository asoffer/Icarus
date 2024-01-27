#ifndef ICARUS_COMMON_INTEGER_H
#define ICARUS_COMMON_INTEGER_H

#include <cstdint>

#include "common/strong_identifier_type.h"
#include "jasmin/serialize/reader.h"
#include "jasmin/serialize/writer.h"
#include "nth/container/flyweight_set.h"
#include "nth/debug/debug.h"

namespace ic {
namespace internal_integer {

// TODO: Actually represent arbitrary-precision.
inline nth::flyweight_set<int64_t> integers;

inline uint32_t InsertAndMark(int64_t n) {
  auto index = integers.index(integers.insert(n).first);
  NTH_REQUIRE((v.harden), index < (uint32_t{1} << 23));
  return (uint32_t{1} << 23) | index;
}

}  // namespace internal_integer

// Represents an arbitrary-precision integer in a program.
struct Integer : private StrongIdentifierType<Integer, uint32_t> {
 private:
  static constexpr uint32_t InlineLimit = uint32_t{1} << 23;
  struct raw_t {};

  explicit Integer(raw_t, uint32_t n) : StrongIdentifierType(n) {}

 public:
  Integer() : Integer(0u) {}

  // If the value numerically lies in the range [-2^23, 2^23), it is
  // represented inline. Otherwise its value is stored in
  // `internal_integer::integers` and an index into that set is stored along
  // with a set high-bit to distinguish this case.
  template <std::unsigned_integral N>
  Integer(N n) requires(std::numeric_limits<N>::max() < InlineLimit)
      : StrongIdentifierType(static_cast<uint32_t>(n)) {}
  template <std::unsigned_integral N>
  Integer(N n) requires(std::numeric_limits<N>::max() >= InlineLimit)
      : StrongIdentifierType((n < static_cast<N>(InlineLimit))
                                 ? static_cast<uint32_t>(n)
                                 : internal_integer::InsertAndMark(n)) {}

  template <std::signed_integral N>
  Integer(N n) requires(std::numeric_limits<N>::max() < InlineLimit)
      : StrongIdentifierType(n >= 0 ? static_cast<uint32_t>(n)
                                    : internal_integer::InsertAndMark(n)) {}
  template <std::signed_integral N>
  Integer(N n) requires(std::numeric_limits<N>::max() >= InlineLimit)
      : StrongIdentifierType((n < static_cast<N>(InlineLimit) and n >= 0)
                                 ? static_cast<uint32_t>(n)
                                 : internal_integer::InsertAndMark(n)) {}

  static Integer FromRepresentation(uint32_t n) { return Integer(raw_t{}, n); }
  static uint32_t ToRepresentation(Integer n) { return n.value(); }

  friend void JasminSerialize(jasmin::Writer auto&, Integer) {
    NTH_UNIMPLEMENTED();
  }
  friend bool JasminDeserialize(jasmin::Reader auto&, Integer&) {
    NTH_UNIMPLEMENTED();
  }

  friend bool operator==(Integer lhs, Integer rhs) {
    return lhs.value() == rhs.value();
  }

  friend bool operator!=(Integer lhs, Integer rhs) {
    return lhs.value() != rhs.value();
  }

  friend bool operator<(Integer lhs, Integer rhs) {
    if (lhs.value() >= InlineLimit) {
      if (rhs.value() < InlineLimit) { return false; }
      return internal_integer::integers.from_index(lhs.value() &
                                                   (InlineLimit - 1)) <
             internal_integer::integers.from_index(rhs.value() &
                                                   (InlineLimit - 1));
    } else if (rhs.value() >= InlineLimit) {
      return true;
    } else {
      return lhs.value() < rhs.value();
    }
  }
  friend bool operator<=(Integer lhs, Integer rhs) { return not(rhs < lhs); }
  friend bool operator>(Integer lhs, Integer rhs) { return rhs < lhs; }
  friend bool operator>=(Integer lhs, Integer rhs) { return rhs <= lhs; }

  Integer operator-() const {
    // TODO: Negating the most negative case isn't handled here properly.
    if (value() >= InlineLimit) {
      return Integer(
          -internal_integer::integers.from_index(value() & (InlineLimit - 1)));
    } else {
      return Integer(-static_cast<int64_t>(value()));
    }
  }

  template <typename H>
  friend H AbslHashValue(H h, Integer n) {
    return H::combine(std::move(h), n.value());
  }

  friend void NthPrint(auto &p, auto &f, Integer const &i) {
    if (i.value() >= InlineLimit) {
      f(p,
        internal_integer::integers.from_index(i.value() & (InlineLimit - 1)));
    } else {
      f(p, i.value());
    }
  }

 private:
};

}  // namespace ic

#endif  // ICARUS_COMMON_INTEGER_H
