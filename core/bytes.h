#ifndef ICARUS_CORE_BYTES_H
#define ICARUS_CORE_BYTES_H

namespace core {

struct Bytes {
  constexpr explicit Bytes(size_t val) : value_(val) {}

  constexpr auto value() const { return value_; }

  constexpr Bytes& operator+=(Bytes b) {
    value_ += b.value_;
    return *this;
  }

 private:
  size_t value_ = 0;
};

constexpr Bytes operator*(Bytes b, size_t n) { return Bytes{b.value() * n}; }
constexpr Bytes operator*(size_t n, Bytes b) { return Bytes{b.value() * n}; }

constexpr Bytes operator+(Bytes lhs, Bytes rhs) {
  return Bytes{lhs.value() + rhs.value()};
}

constexpr bool operator==(Bytes lhs, Bytes rhs) {
  return lhs.value() == rhs.value();
}

constexpr bool operator!=(Bytes lhs, Bytes rhs) { return !(lhs == rhs); }
constexpr bool operator<(Bytes lhs, Bytes rhs) {
  return lhs.value() < rhs.value();
}

constexpr bool operator<=(Bytes lhs, Bytes rhs) { return !(rhs < lhs); }
constexpr bool operator>(Bytes lhs, Bytes rhs) { return rhs < lhs; }
constexpr bool operator>=(Bytes lhs, Bytes rhs) { return !(lhs < rhs); }

inline std::ostream& operator<<(std::ostream& os, Bytes b) {
  return os << b.value() << " bytes";
}

}  // namespace core

#endif  // ICARUS_CORE_BYTES_H
