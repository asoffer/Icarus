#ifndef ICARUS_AST_SCOPE_MODULE_H
#define ICARUS_AST_SCOPE_MODULE_H

#include "ast/scope/decl.h"

namespace module {
struct BasicModule;
}  // namespace module

namespace ast {

// A declarative scope which is always at the root of a scope hierarchy. All
// program contents lie in exactly one module.
struct ModuleScope : public DeclScope {
  ModuleScope(module::BasicModule *mod) : DeclScope(nullptr), module_(mod) {}

  module::BasicModule *module() { return module_; }
  module::BasicModule const *module() const { return module_; }

 private:
  friend struct Scope;
  module::BasicModule *module_;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_MODULE_H
