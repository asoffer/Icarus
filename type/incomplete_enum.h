#ifndef ICARUS_TYPE_INCOMPLETE_ENUM_H
#define ICARUS_TYPE_INCOMPLETE_ENUM_H

#include <optional>
#include <string_view>

#include "base/container/vector.h"
#include "ir/register.h"

struct Module;

namespace type {
struct Enum;

struct IncompleteEnum {
  IncompleteEnum(::Module const *mod);
  void add(std::string_view s);
  void set_last_value(i32 value);

  Enum const *finalize() &&;

 private:
  ::Module const *mod_;
  base::vector<std::pair<std::string, std::optional<i32>>> entries_;
};

}  // namespace type

#endif  // ICARUS_TYPE_INCOMPLETE_ENUM_H
