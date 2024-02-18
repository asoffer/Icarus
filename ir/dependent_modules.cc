#include "ir/dependent_modules.h"

#include "nth/debug/debug.h"

namespace ic {

Module const& DependentModules::operator[](ModuleId id) const {
  switch (id.value()) {
    case ModuleId::Builtin().value(): return builtin_;
    case ModuleId::Foreign().value(): return foreign_;
    default:
      NTH_REQUIRE((v.harden), id.value() < modules_.size());
      return modules_[id.value()];
  }
}

Module& DependentModules::add(std::string&&) { return modules_.emplace_back(); }
Module& DependentModules::add(std::string const&) {
  return modules_.emplace_back();
}
Module& DependentModules::add(std::string_view) {
  return modules_.emplace_back();
}

}  // namespace ic
