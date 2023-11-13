#ifndef ICARUS_COMMON_MODULE_MAP_H
#define ICARUS_COMMON_MODULE_MAP_H

#include <string>
#include <string_view>

#include "common/module_id.h"
#include "nth/container/flyweight_set.h"

namespace ic {

struct ModuleMap {
  ModuleId add(std::string const& module_name);
  ModuleId add(std::string&& module_name);
  ModuleId add(std::string_view module_name);

  ModuleId operator[](std::string_view module_name) const;

  auto begin() const { return by_name_.begin(); }
  auto end() const { return by_name_.end(); }

 private:
  nth::flyweight_set<std::string> by_name_ = {"~builtin~"};
};

}  // namespace ic

#endif  // ICARUS_COMMON_MODULE_MAP_H
