#ifndef ICARUS_IR_VALUE_LABEL_H
#define ICARUS_IR_VALUE_LABEL_H

#include <string>

namespace ir {

struct Label {
  explicit constexpr Label() : label_(nullptr) {}
  explicit Label(std::string const *label) : label_(ASSERT_NOT_NULL(label)) {}

  constexpr std::string const *get() const { return label_; }
  constexpr std::string const &operator*() const { return *label_; }
  constexpr std::string const *operator->() const { return label_; }

  friend bool operator==(Label lhs, Label rhs) {
    return lhs.label_ == rhs.label_;
  }

  friend bool operator!=(Label lhs, Label rhs) { return not(lhs == rhs); }

  template <typename H>
  friend H AbslHashValue(H h, Label l) {
    return H::combine(std::move(h), l.label_);
  }

  friend std::ostream &operator<<(std::ostream &os, Label l) {
    return os << "Label(" << *l << ")";
  }

 private:
  std::string const *label_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_LABEL_H
