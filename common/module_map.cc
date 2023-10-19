#include "common/module_map.h"

#include <utility>

namespace ic {

ModuleId ModuleMap::add(std::string&& module_name) {
  auto [iter, inserted] = by_name_.insert(std::move(module_name));
  return ModuleId(by_name_.index(iter));
}

ModuleId ModuleMap::add(std::string const& module_name) {
  auto [iter, inserted] = by_name_.insert(module_name);
  return ModuleId(by_name_.index(iter));
}

ModuleId ModuleMap::add(std::string_view module_name) {
  // TODO: Enable heterogenous lookup for `nth::flyweight_set`.
  auto [iter, inserted] = by_name_.insert(std::string(module_name));
  return ModuleId(by_name_.index(iter));
}

ModuleId ModuleMap::operator[](std::string_view module_name) const {
  // TODO: Enable heterogenous lookup for `nth::flyweight_set`.
  auto iter = by_name_.find(std::string(module_name));
  if (iter == by_name_.end()) { return ModuleId::Invalid(); }
  return ModuleId(by_name_.index(iter));
}

}  // namespace ic
