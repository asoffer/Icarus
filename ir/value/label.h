#ifndef ICARUS_IR_VALUE_LABEL_H
#define ICARUS_IR_VALUE_LABEL_H

#include <string_view>

#include "base/extend.h"
#include "base/extend/absl_hash.h"

namespace ir {

struct Label : base::Extend<Label, 1>::With<base::AbslHashExtension> {
  explicit constexpr Label() : label_(nullptr) {}
  explicit Label(std::string_view const *label) : label_(ASSERT_NOT_NULL(label)) {}

  constexpr std::string_view const *get() const { return label_; }
  constexpr std::string_view const &operator*() const { return *label_; }
  constexpr std::string_view const *operator->() const { return label_; }

  friend std::ostream &operator<<(std::ostream &os, Label l) {
    return os << "Label(" << *l << ")";
  }

 private:
  friend base::EnableExtensions;

  std::string_view const *label_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_LABEL_H
