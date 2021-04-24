#ifndef ICARUS_TYPE_INTERFACE_INTERFACE_H
#define ICARUS_TYPE_INTERFACE_INTERFACE_H

#include <iostream>

#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "type/type.h"

namespace interface {

struct Interface : base::Extend<Interface, 1>::With<base::AbslHashExtension> {
  // TODO: Remove this.
  constexpr Interface() = default;

  static Interface Just(type::Type t);
  static Interface ConvertsTo(type::Type t);

  friend std::ostream& operator<<(std::ostream& os, Interface) {
    // TODO
    return os << "interface";
  }

  bool SatisfiedBy(type::Type t) const;

  struct Impl;

 private:
  friend base::EnableExtensions;

  explicit Interface(Impl const* impl) : impl_(impl) {}

  Impl const* impl_ = nullptr;
};

}  // namespace interface

#endif  // ICARUS_TYPE_INTERFACE_INTERFACE_H
