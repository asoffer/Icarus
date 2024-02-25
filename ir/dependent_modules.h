#ifndef ICARUS_IR_DEPENDENT_MODULES_H
#define ICARUS_IR_DEPENDENT_MODULES_H

#include <string>
#include <string_view>

#include "common/module_id.h"
#include "ir/module.h"

namespace ic {

struct DependentModules {
  DependentModules() = default;
  Module &add(std::string &&name);
  Module &add(std::string const &name);
  Module &add(std::string_view name);

  Module const &operator[](ModuleId id) const;

  Module &foreign_module() { return foreign_; }
  Module const &foreign_module() const { return foreign_; }

  size_t count() const { return modules_.size(); }

 private:
  // All module that the currently-being-compiled module depends on
  // transitively. Modules are ordered according to some topological sorting
  // so that if `b` depends on `a`, then `a` appears before `b`.
  std::vector<Module> modules_;
  Module foreign_;
};

}  // namespace ic

#endif  // ICARUS_IR_DEPENDENT_MODULES_H
