#ifndef ICARUS_TYPE_CALLABLE_H
#define ICARUS_TYPE_CALLABLE_H

#include <vector>

#include "core/arguments.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {

struct Callable : LegacyType {
  Callable()
      : LegacyType(LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 1,
                                     .is_movable               = 1,
                                     .has_destructor           = 0}) {}

  virtual std::vector<type::Type> return_types(
      core::Arguments<type::Typed<ir::Value>> const& args) const = 0;
};

}  // namespace type

#endif  // ICARUS_TYPE_CALLABLE_H
