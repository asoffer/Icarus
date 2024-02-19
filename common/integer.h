#ifndef ICARUS_COMMON_INTEGER_H
#define ICARUS_COMMON_INTEGER_H

#include <cstdint>

#include "common/strong_identifier_type.h"
#include "nth/container/flyweight_set.h"
#include "nth/debug/debug.h"
#include "nth/io/serialize/deserialize.h"
#include "nth/io/serialize/serialize.h"
#include "nth/numeric/integer.h"

namespace ic {
namespace internal_integer {

inline nth::flyweight_set<nth::integer> integers;

inline uint32_t InsertAndMark(nth::integer n) {
  auto index = integers.index(integers.insert(std::move(n)).first);
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
  Integer(nth::integer n)
      : StrongIdentifierType(
            (not nth::negative(n) and n < InlineLimit)
                ? static_cast<uint32_t>(n)
                : internal_integer::InsertAndMark(std::move(n))) {}
  Integer(std::integral auto n) : Integer(nth::integer(n)) {}

  static Integer FromRepresentation(uint32_t n) { return Integer(raw_t{}, n); }
  static uint32_t ToRepresentation(Integer n) { return n.value(); }

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

  friend bool NthSerialize(auto &s, Integer n) {
    if (n.value() >= InlineLimit) {
      nth::integer value =
          internal_integer::integers.from_index(n.value() & (InlineLimit - 1));
      return nth::io::serialize(s, value);
    } else {
      return nth::io::serialize(s, nth::integer(n.value()));
    }
  }

  friend bool NthDeserialize(auto &d, Integer &n) {
    nth::integer num;
    if (not nth::io::deserialize(d, num)) { return false; }
    n = Integer(num);
    return true;
  }
};

}  // namespace ic

#endif  // ICARUS_COMMON_INTEGER_H
