#ifndef ICARUS_TYPE_INCOMPLETE_FLAGS_H
#define ICARUS_TYPE_INCOMPLETE_FLAGS_H

#include <optional>
#include <random>
#include <string_view>
#include <utility>

#include "base/container/vector.h"
#include "ir/register.h"

struct Module;

namespace type {
struct Flags;

struct IncompleteFlags {
  IncompleteFlags(::Module const *mod);

  void add(std::string_view s);
  void set_last_value(i32 value);

  Flags const *finalize() &&;

 private:
  ::Module const *mod_;
  base::vector<std::pair<std::string, std::optional<i32>>> entries_;
  size_t All = 0;
};

}  // namespace type

#endif  // ICARUS_TYPE_INCOMPLETE_FLAGS_H
