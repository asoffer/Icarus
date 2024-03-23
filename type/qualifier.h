#ifndef ICARUS_TYPE_QUALIFIER_H
#define ICARUS_TYPE_QUALIFIER_H

#include <utility>

namespace ic::type {

struct Qualifier {
  static constexpr Qualifier Addressable() { return Qualifier(2); }
  static constexpr Qualifier Constant() { return Qualifier(1); }
  static constexpr Qualifier Unqualified() { return Qualifier(0); }

  friend constexpr bool operator==(Qualifier, Qualifier) = default;
  friend constexpr bool operator!=(Qualifier, Qualifier) = default;
  friend constexpr bool operator<=(Qualifier lhs, Qualifier rhs) {
    return (lhs.data_ & rhs.data_) == lhs.data_;
  }
  friend constexpr bool operator>=(Qualifier lhs, Qualifier rhs) {
    return rhs <= lhs;
  }
  constexpr Qualifier& operator|=(Qualifier q) {
    data_ |= q.data_;
    return *this;
  }

  template <typename H>
  friend H AbslHashValue(H h, Qualifier q) {
    H::combine(std::move(h), q.data_);
  }

  friend void NthPrint(auto& p, auto&, Qualifier q) {
    if (q.data_ == 1) { p.write("c"); }
    if (q.data_ == 2) { p.write("a"); }
  }

 private:
  friend struct QualifiedType;
  explicit constexpr Qualifier(uint8_t data = 0) : data_(data) {}

  uint8_t data_;
};

}  // namespace ic::type

#endif // ICARUS_TYPE_QUALIFIER_H
