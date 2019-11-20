#ifndef ICARUS_AST_SCOPE_MODULE_H
#define ICARUS_AST_SCOPE_MODULE_H

#include "ast/scope/fn.h"

namespace module {
struct BasicModule;
}  // namespace module

namespace ast {

// TODO using FnScope for modules makes sense for executable modules and isn't
// super harmful for library modules, but also isn't entirely correct. Figure
// out the semantically correct solution here, probably extracting some of the
// functionality of FnScope.
struct ModuleScope : public FnScope {
  ModuleScope(module::BasicModule *mod) : FnScope(nullptr), module_(mod) {}

  module::BasicModule *module() { return module_; }
  module::BasicModule const *module() const { return module_; }

 private:
  friend struct Scope;
  module::BasicModule *module_;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_MODULE_H
