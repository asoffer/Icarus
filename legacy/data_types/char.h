#ifndef ICARUS_DATA_TYPES_CHAR_H
#define ICARUS_DATA_TYPES_CHAR_H

#include <concepts>
#include <ostream>
#include <utility>

namespace data_types {

// We use `int8_t` and `uint8_t` to represent signed and unsigned integers
// respectively. Conformant C++ implementations may have either of these (but
// not both) be aliases for `char`, so we cannot use `char` directly as a
// character type.
struct Char {
  Char() : data_(0) {}

  template <std::integral T>
  requires(sizeof(T) == 1) Char(T c) : data_(c) {}

  template <std::integral T>
  operator T() const {
    return data_;
  }

  template <std::integral T>
  T as_type() const {
    return data_;
  }

  template <typename H>
  friend H AbslHashValue(H h, Char c) {
    return H::combine(std::move(h), c.data_);
  }

  auto operator<=>(Char const&) const = default;

  friend std::ostream& operator<<(std::ostream& os, Char c) {
    return os << "'" << c.data_ << "'";
  }

 private:
  char data_;
};

}  // namespace data_types

#endif  //  ICARUS_DATA_TYPES_CHAR_H
