#ifndef ICARUS_AST_EXPR_PTR_H
#define ICARUS_AST_EXPR_PTR_H

#include <iostream>

namespace ast {
struct Expression;

// TODO pick a better name
struct ExprPtr {
  constexpr ExprPtr(Expression const *expr, int8_t offset = 0)
      : value_(reinterpret_cast<uintptr_t>(expr) | (offset & 0x03)) {}

  template <typename H>
  friend H AbslHashValue(H h, ExprPtr e) {
    return H::combine(std::move(h), e.value_);
  }

  friend constexpr bool operator==(ExprPtr lhs, ExprPtr rhs) {
    return lhs.value_ == rhs.value_;
  }

  Expression *get() const {
    return (value_ & 0x1) ? nullptr : reinterpret_cast<Expression *>(value_);
  }

  friend std::ostream &operator<<(std::ostream &os, ExprPtr e) {
    return os << "expr-ptr(" << e.value_ << ")";
  }

 private:
  uintptr_t value_;
};

}  // namespace ast

#endif  // ICARUS_AST_EXPR_PTR_H
