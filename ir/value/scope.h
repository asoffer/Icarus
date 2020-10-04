#ifndef ICARUS_IR_VALUE_SCOPE_H
#define ICARUS_IR_VALUE_SCOPE_H

#include <iostream>

namespace ir {
struct CompiledScope;

struct Scope {
  constexpr Scope() : Scope(nullptr) {}
  explicit constexpr Scope(CompiledScope *scope) : scope_(scope) {}

  friend bool operator==(Scope lhs, Scope rhs) {
    return lhs.scope_ == rhs.scope_;
  }
  friend bool operator!=(Scope lhs, Scope rhs) { return not(lhs == rhs); }

  template <typename H>
  friend H AbslHashValue(H h, Scope j) {
    return H::combine(std::move(h), j.scope_);
  }

  friend std::ostream &operator<<(std::ostream &os, Scope s) {
    return os << "Scope(" << s.scope_ << ")";
  }

 private:
  friend CompiledScope;
  CompiledScope *scope_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_SCOPE_H
