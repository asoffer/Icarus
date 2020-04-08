#ifndef ICARUS_TYPE_CALLABLE_H
#define ICARUS_TYPE_CALLABLE_H

#include <vector>

#include "core/fn_args.h"
#include "ir/results.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace type {

struct Callable : Type {
  ~Callable() override {};
  virtual std::vector<type::Type const*> return_types(
      core::FnArgs<type::Typed<ir::Results>> const& args) const = 0;
};

}  // namespace type

#endif  // ICARUS_TYPE_CALLABLE_H
