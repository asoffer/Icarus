#ifndef ICARUS_IR_VALUE_INTEGER_H
#define ICARUS_IR_VALUE_INTEGER_H

#include <cstdint>
#include <iostream>
#include <string>
#include <utility>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "base/serialize.h"

namespace ir {

// TODO: Implement arbitrary-precision integers
struct Integer {
  Integer(int64_t n = 0) : data_(n) {}
  ~Integer() {}

  Integer(Integer const &) = default;
  Integer(Integer &&)      = default;
  Integer &operator=(Integer const &) = default;
  Integer &operator=(Integer &&)          = default;
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

  friend void BaseSerialize(auto &s, Integer n) { base::Serialize(s, n.data_); }
  friend bool BaseDeserialize(auto &d, Integer &n) {
    return base::Deserialize(d, n.data_);
  }

  friend std::ostream &operator<<(std::ostream &os, Integer const &n) {
    absl::Format(&os, "%d", n);
    return os;
  }

  template <std::integral T>
  T as_type() const {
    return data_;
  }

  template <std::floating_point T>
  T as_type() const {
    return data_;
  }

  // TODO: Remove
  int64_t value() const { return data_; }

 private:
  int64_t data_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_INTEGER_H
