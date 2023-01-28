#ifndef ICARUS_DATA_TYPES_LABEL_H
#define ICARUS_DATA_TYPES_LABEL_H

#include <string>

#include "base/extend.h"
#include "base/extend/absl_hash.h"

namespace data_types {

struct Label : base::Extend<Label, 1>::With<base::AbslHashExtension> {
  explicit constexpr Label() : label_(nullptr) {}
  explicit Label(std::string const *label) : label_(ASSERT_NOT_NULL(label)) {}

  constexpr std::string const *get() const { return label_; }
  constexpr std::string const &operator*() const { return *label_; }
  constexpr std::string const *operator->() const { return label_; }

  friend std::ostream &operator<<(std::ostream &os, Label l) {
    return os << "Label(" << *l << ")";
  }

 private:
  friend base::EnableExtensions;

  std::string const *label_;
};

}  // namespace data_types

#endif  // ICARUS_DATA_TYPES_LABEL_H
