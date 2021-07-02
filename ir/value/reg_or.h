#ifndef ICARUS_IR_VALUE_REG_OR_H
#define ICARUS_IR_VALUE_REG_OR_H

#include "base/stringify.h"
#include "ir/value/reg.h"

namespace ir {

// RegOr<T>:
// Represents a register in the intermediate representation, or an immediate
// value of the given type. This is a common variant-type. For instance, when
// building the intermediate representation, if we see an expression of the form
// `a + b`, this will generate a new register unless both `a` and `b` are
// immediate values, in which case, we can constant-fold them.
template <typename T>
struct RegOr {
  using type = T;
  static_assert(not std::is_same_v<Reg, type>);
  static_assert(std::is_trivially_copyable_v<type>);

  template <bool B                   = std::is_default_constructible_v<type>,
            std::enable_if_t<B, int> = 0>
  constexpr RegOr() : val_{}, is_reg_(false) {}
  constexpr RegOr(Reg reg) : reg_(reg), is_reg_(true) {}
  constexpr RegOr(type val) : val_(val), is_reg_(false) {}

  constexpr bool is_reg() const { return is_reg_; }

  Reg &reg() {
    ASSERT(is_reg() == true);
    return reg_;
  }

  Reg reg() const {
    ASSERT(is_reg() == true);
    return reg_;
  }

  type value() const {
    ASSERT(is_reg() == false);
    return val_;
  }

  template <typename Fn>
  type resolve(Fn &&fn) const {
    return is_reg() ? std::forward<Fn>(fn)(reg()) : value();
  }

  template <typename Fn>
  auto apply(Fn &&fn) const {
    return is_reg() ? std::forward<Fn>(fn)(reg())
                    : std::forward<Fn>(fn)(value());
  }

  template <typename H>
  friend H AbslHashValue(H h, RegOr<T> r) {
    return r.is_reg() ? H::combine(std::move(h), true, r.reg())
                      : H::combine(std::move(h), false, r.value());
  }

  friend std::string stringify(RegOr const &r) {
    using base::stringify;
    return r.is_reg() ? stringify(r.reg()) : stringify(r.value());
  }

 private:
  union {
    Reg reg_;
    type val_;
  };

  bool is_reg_;
};

template <typename T>
bool operator==(RegOr<T> const &lhs, RegOr<T> const &rhs) {
  if (lhs.is_reg()) { return rhs.is_reg() and lhs.reg() == rhs.reg(); }
  return not rhs.is_reg() and lhs.value() == rhs.value();
}

template <typename T>
bool operator!=(RegOr<T> const &lhs, RegOr<T> const &rhs) {
  return not(lhs == rhs);
}

}  // namespace ir

#endif  // ICARUS_IR_VALUE_REG_OR_H
