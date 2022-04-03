#ifndef ICARUS_TYPE_ARGUEMENT_H
#define ICARUS_TYPE_ARGUEMENT_H

#include <string_view>

#include "base/untyped_buffer_view.h"
#include "type/type.h"

namespace type {

struct Argument {
  std::string_view name;
  type::Type type;
  base::untyped_buffer_view value = base::untyped_buffer_view();
};

}  // namespace type

#endif  // ICARUS_TYPE_ARGUEMENT_H
