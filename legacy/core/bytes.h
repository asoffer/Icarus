#ifndef ICARUS_CORE_BYTES_H
#define ICARUS_CORE_BYTES_H

#include <iostream>

namespace core {

struct Bytes {
  template <typename T>
  static constexpr Bytes Get() {
    return Bytes{sizeof(T)};
  }

  constexpr explicit Bytes(int64_t val = 0) : value_(val) {}

  constexpr auto value() const { return value_; }

  constexpr Bytes& operator+=(Bytes b) {
    value_ += b.value_;
    return *this;
  }

 private:
  int64_t value_ = 0;
};

constexpr Bytes operator*(Bytes b, int64_t n) { return Bytes{b.value() * n}; }
constexpr Bytes operator*(int64_t n, Bytes b) { return Bytes{b.value() * n}; }

constexpr Bytes operator+(Bytes lhs, Bytes rhs) {
  return Bytes{lhs.value() + rhs.value()};
}

constexpr bool operator==(Bytes lhs, Bytes rhs) {
  return lhs.value() == rhs.value();
}

constexpr bool operator!=(Bytes lhs, Bytes rhs) { return not(lhs == rhs); }
constexpr bool operator<(Bytes lhs, Bytes rhs) {
  return lhs.value() < rhs.value();
}

constexpr bool operator<=(Bytes lhs, Bytes rhs) { return not(rhs < lhs); }
constexpr bool operator>(Bytes lhs, Bytes rhs) { return rhs < lhs; }
constexpr bool operator>=(Bytes lhs, Bytes rhs) { return not(lhs < rhs); }

constexpr int64_t operator/(Bytes lhs, Bytes rhs) {
  return lhs.value() / rhs.value();
}

inline std::ostream& operator<<(std::ostream& os, Bytes b) {
  return os << b.value() << " bytes";
}

}  // namespace core

#endif  // ICARUS_CORE_BYTES_H
