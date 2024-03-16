#ifndef ICARUS_COMMON_INTEGER_H
#define ICARUS_COMMON_INTEGER_H

#include "common/internal/constant_handle.h"
#include "nth/numeric/integer.h"

namespace ic {
// Represents an arbitrary-precision integer in a program.
struct Integer : internal_constants::ConstantHandle<Integer> {
  using backing_type = nth::integer;

  Integer() : Integer(0u) {}
  Integer(std::integral auto n) : Integer(nth::integer(n)) {}
  Integer(nth::integer const& n);
  Integer(nth::integer&& n);

  static Integer FromRepresentation(uint32_t n) { return Integer(raw_t{}, n); }
  static uint32_t ToRepresentation(Integer n) { return n.value(); }

  friend bool operator<(Integer lhs, Integer rhs) { return LessThan(lhs, rhs); }
  friend bool operator<=(Integer lhs, Integer rhs) { return not(rhs < lhs); }
  friend bool operator>(Integer lhs, Integer rhs) { return rhs < lhs; }
  friend bool operator>=(Integer lhs, Integer rhs) { return rhs <= lhs; }

  Integer operator-() const;

  explicit operator nth::integer const&() const;

 private:
  struct raw_t {};

  static bool LessThan(Integer lhs, Integer rhs);
  explicit Integer(raw_t, uint32_t n) : ConstantHandle(n) {}
};

}  // namespace ic

#endif  // ICARUS_COMMON_INTEGER_H
