#ifndef ICARUS_COMMON_INTEGER_H
#define ICARUS_COMMON_INTEGER_H

#include "common/internal/constant_handle.h"
#include "common/result.h"
#include "nth/numeric/integer.h"

namespace ic {

// Represents an arbitrary-precision integer in a program.
struct Integer : internal_constants::ConstantHandle<Integer> {
  using backing_type = nth::integer;

  Integer() : Integer(0u) {}
  Integer(std::integral auto n) : Integer(nth::integer(n)) {}
  Integer(nth::integer const& n);
  Integer(nth::integer&& n);

  friend bool operator<(Integer lhs, Integer rhs) { return LessThan(lhs, rhs); }
  friend bool operator<=(Integer lhs, Integer rhs) { return not(rhs < lhs); }
  friend bool operator>(Integer lhs, Integer rhs) { return rhs < lhs; }
  friend bool operator>=(Integer lhs, Integer rhs) { return rhs <= lhs; }

  Integer operator-() const;

  explicit operator nth::integer const&() const;

 private:
  friend ConstantHandle;
  static bool LessThan(Integer lhs, Integer rhs);
  explicit Integer(internal_constants::from_representation_t, uint32_t n)
      : ConstantHandle(n) {}
};

}  // namespace ic

#endif  // ICARUS_COMMON_INTEGER_H
