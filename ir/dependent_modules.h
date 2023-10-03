#ifndef ICARUS_IR_DEPENDENT_MODULES_H
#define ICARUS_IR_DEPENDENT_MODULES_H

#include <optional>
#include <span>
#include <vector>

#include "ir/module.h"
#include "ir/module_id.h"

namespace ic {

struct DependentModules {
  Module const& operator[](ModuleId id) const;

  // Returns a reference to the builtin module.
  Module const &builtin_module() const { return modules_[0]; }

  size_t count() const { return modules_.size(); }

 private:
  // TODO: Expose what's necessary.
  friend struct Deserializer;

  // All module that the currently-being-compiled module depends on
  // transitively. Modules are ordered according to some topological sorting
  // so that if `b` depends on `a`, then `a` appears before `b`.
  std::vector<Module> modules_;
};

}  // namespace ir

#endif  // ICARUS_IR_DEPENDENT_MODULES_H
