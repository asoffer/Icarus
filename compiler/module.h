#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include <memory>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "absl/synchronization/notification.h"
#include "ast/ast_fwd.h"
#include "base/guarded.h"
#include "base/no_destructor.h"
#include "compiler/context.h"
#include "ir/compiled_fn.h"
#include "ir/compiled_jump.h"
#include "module/module.h"
#include "type/type_fwd.h"

namespace compiler {

struct CompiledModule : module::BasicModule {
  explicit CompiledModule() : data_(this) {}
  ~CompiledModule() override {}

  ir::Value ExportedValue(ast::Declaration::Id const *id) const {
    return context().LoadConstant(id);
  }

  // If we're requesting from a different module we need to ensure that we've
  // waited for that module to complete processing. But from the same module we
  // node processing order to dictates safety.
  Context const &context(module::BasicModule const *requestor) const {
    if (requestor != this) { notification_.WaitForNotification(); }
    return data_;
  }
  Context &context(module::BasicModule const *requestor) {
    // TODO: We really probably want to assert if it's a different module. You
    // shouldn't be able to modify the context of a different module.
    if (requestor != this) { notification_.WaitForNotification(); }
    return data_;
  }
  Context const &context() const { return context(this); }
  Context &context() { return context(this); }

  bool has_error_in_dependent_module() const {
    return depends_on_module_with_errors_;
  }
  void set_dependent_module_with_errors() {
    depends_on_module_with_errors_ = true;
  }

 protected:
  // Child classes must call this when compilation of this module is complete
  // to notify other modules which may be waiting on data for their own
  // compilation.
  void CompilationComplete() { notification_.Notify(); }

 private:
  Context data_;
  absl::Notification notification_;

  // This flag should be set to true if this module is ever found to depend on
  // another which has errors, even if those errors do not effect
  // code-generation in this module.
  bool depends_on_module_with_errors_ = false;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
