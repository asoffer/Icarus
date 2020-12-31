#ifndef ICARUS_IR_VALUE_CHAR_H
#define ICARUS_IR_VALUE_CHAR_H

#include <concepts>
#include <iostream>
#include <type_traits>

namespace ir {

// We use `int8_t` and `uint8_t` to represent signed and unsigned integers
// respectively. Conformant C++ implementations may have either of these (but
// not both) be aliases for `char`, so we cannot use `char` directly as a
// character type.
struct Char {
  Char() : data_(0) {}

  template <std::integral T, std::enable_if_t<sizeof(T) == 1, int> = 0>
  /* implicit */ Char(T c) : data_(c) {}

  template <std::integral T>
  operator T() {
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

}  // namespace ir

#endif  //  ICARUS_IR_VALUE_CHAR_H
