#include "ir/dependent_modules.h"

#include "nth/debug/debug.h"

namespace ic {

Module const& DependentModules::operator[](ModuleId id) const {
  NTH_REQUIRE(id.value() < modules_.size());
  return modules_[id.value()];
}

}  // namespace ic
