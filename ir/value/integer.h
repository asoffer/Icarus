#ifndef ICARUS_IR_VALUE_INTEGER_H
#define ICARUS_IR_VALUE_INTEGER_H

#include <cstdint>
#include <iostream>
#include <string>
#include <utility>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"

namespace ir {

// TODO: In the short-term we're just going to use i64 for constant integer
// types. Long-term we would like to allow arbitrary precision computation at
// compile-time, but the semantics here are subtle and difficult and not
// important enough to tackle just yet.
using Integer = int64_t;

// TODO: Implement arbitrary-precision integers
#if 0
struct Integer {
  Integer(int64_t n = 0) : data_(n) {}

  auto operator<=>(Integer const &) const = default;

  template <typename H>
  friend H AbslHashValue(H h, Integer const &n) {
    return H::combine(std::move(h), n.data_);
  }

  Integer operator-() const { return Integer(-data_); }

  friend Integer operator+(Integer const &lhs, Integer const &rhs) {
    return Integer(lhs.data_ + rhs.data_);
  }
  friend Integer operator-(Integer const &lhs, Integer const &rhs) {
    return Integer(lhs.data_ - rhs.data_);
  }
  friend Integer operator*(Integer const &lhs, Integer const &rhs) {
    return Integer(lhs.data_ * rhs.data_);
  }
  friend Integer operator/(Integer const &lhs, Integer const &rhs) {
    return Integer(lhs.data_ / rhs.data_);
  }
  friend Integer operator%(Integer const &lhs, Integer const &rhs) {
    return Integer(lhs.data_ % rhs.data_);
  }

  friend absl::FormatConvertResult<absl::FormatConversionCharSet::kIntegral>
  AbslFormatConvert(Integer const &n, const absl::FormatConversionSpec &spec,
                    absl::FormatSink *s) {
    s->Append(absl::StrCat(n.data_));
    return {true};
  }

  friend std::ostream &operator<<(std::ostream &os, Integer const &n) {
    absl::Format(&os, "%d", n);
    return os;
  }

  template <std::integral T>
  T as_type() {
    return data_;
  }

  template <std::floating_point T>
  T as_type() {
    return data_;
  }

  // TODO: Remove
  int64_t value() const { return data_; }

 private:
  int64_t data_;
};
#endif

}  // namespace ir

#endif  // ICARUS_IR_VALUE_INTEGER_H
