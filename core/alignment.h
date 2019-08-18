#ifndef ICARUS_CORE_ALIGNMENT_H
#define ICARUS_CORE_ALIGNMENT_H

namespace core {

struct Alignment {
  // Returns the alignment on the host.
  // TODO at some point we'll want a better way to spell this so the
  // interpretter and the host can be different.
  template <typename T>
  static constexpr Alignment Get() {
    return Alignment{alignof(T)};
  }

  constexpr explicit Alignment(size_t val) : value_(val) {}

  constexpr auto value() const { return value_; }

 private:
  size_t value_ = 0;
};

constexpr bool operator==(Alignment lhs, Alignment rhs) {
  return lhs.value() == rhs.value();
}

constexpr bool operator!=(Alignment lhs, Alignment rhs) { return !(lhs == rhs); }
constexpr bool operator<(Alignment lhs, Alignment rhs) {
  return lhs.value() < rhs.value();
}

constexpr bool operator<=(Alignment lhs, Alignment rhs) { return !(rhs < lhs); }
constexpr bool operator>(Alignment lhs, Alignment rhs) { return rhs < lhs; }
constexpr bool operator>=(Alignment lhs, Alignment rhs) { return !(lhs < rhs); }

inline std::ostream& operator<<(std::ostream& os, Alignment a) {
  return os << "align(" << a.value() << ")";
}

}  // namespace core

#endif  // ICARUS_CORE_ALIGNMENT_H
