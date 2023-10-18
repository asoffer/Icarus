#ifndef ICARUS_IR_DEPENDENT_MODULES_H
#define ICARUS_IR_DEPENDENT_MODULES_H

#include <optional>
#include <span>
#include <vector>

#include "common/module_id.h"
#include "ir/module.h"

namespace ic {

struct DependentModules {
  Module const& operator[](ModuleId id) const;

  // Returns a reference to the builtin module.
  Module const &builtin_module() const { return modules_[0]; }

  Module &foreign_module() { return foreign_; }
  Module const &foreign_module() const { return foreign_; }

  size_t count() const { return modules_.size(); }

 private:
  // TODO: Expose what's necessary.
  friend struct Deserializer;

  // All module that the currently-being-compiled module depends on
  // transitively. Modules are ordered according to some topological sorting
  // so that if `b` depends on `a`, then `a` appears before `b`.
  std::vector<Module> modules_;
  Module foreign_;
};

}  // namespace ir

#endif  // ICARUS_IR_DEPENDENT_MODULES_H
