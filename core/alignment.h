#ifndef ICARUS_CORE_ALIGNMENT_H
#define ICARUS_CORE_ALIGNMENT_H

#include <iostream>

namespace core {

struct Alignment {
  template <typename T>
  static constexpr Alignment Get() {
    return Alignment{alignof(T)};
  }

  constexpr explicit Alignment(size_t val = 1) : value_(val) {}

  constexpr auto value() const { return value_; }

 private:
  size_t value_ = 0;
};

constexpr bool operator==(Alignment lhs, Alignment rhs) {
  return lhs.value() == rhs.value();
}

constexpr bool operator!=(Alignment lhs, Alignment rhs) {
  return not(lhs == rhs);
}
constexpr bool operator<(Alignment lhs, Alignment rhs) {
  return lhs.value() < rhs.value();
}

constexpr bool operator<=(Alignment lhs, Alignment rhs) {
  return not(rhs < lhs);
}
constexpr bool operator>(Alignment lhs, Alignment rhs) { return rhs < lhs; }
constexpr bool operator>=(Alignment lhs, Alignment rhs) {
  return not(lhs < rhs);
}

inline std::ostream& operator<<(std::ostream& os, Alignment a) {
  return os << "align(" << a.value() << ")";
}

}  // namespace core

#endif  // ICARUS_CORE_ALIGNMENT_H
