#ifndef ICARUS_IR_VALUE_INTERFACE_H
#define ICARUS_IR_VALUE_INTERFACE_H

#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "base/global.h"
#include "core/params.h"
#include "type/type.h"

namespace ir {

struct Interface : base::Extend<Interface, 1>::With<base::AbslHashExtension> {
  // TODO: Remove this.
  constexpr Interface() = default;


  static Interface ConvertsTo(type::Type t);

 private:
  friend base::EnableExtensions;

  explicit Interface(void const *id) : id_(id) {}

  [[maybe_unused]] void const *id_ = nullptr;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_INTERFACE_H
