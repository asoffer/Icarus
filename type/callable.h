#ifndef ICARUS_TYPE_CALLABLE_H
#define ICARUS_TYPE_CALLABLE_H

#include <vector>

#include "core/fn_args.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {

struct Callable : Type {
  Callable()
      : Type(Type::Flags{.is_default_initializable = 0,
                         .is_copyable              = 1,
                         .is_movable               = 1,
                         .has_destructor           = 0}) {}
  ~Callable() override{};

  virtual std::vector<type::Type const*> return_types(
      core::FnArgs<type::Typed<ir::Value>> const& args) const = 0;
};

}  // namespace type

#endif  // ICARUS_TYPE_CALLABLE_H
