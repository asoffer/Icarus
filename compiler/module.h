#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include <memory>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "ast/ast_fwd.h"
#include "base/guarded.h"
#include "base/no_destructor.h"
#include "compiler/data.h"
#include "ir/compiled_fn.h"
#include "ir/jump.h"
#include "module/module.h"
#include "type/type_fwd.h"

namespace compiler {

struct CompiledModule : module::BasicModule {
  explicit CompiledModule() : data_(this) {}
  ~CompiledModule() override {}

  ir::Value ExportedValue(ast::Declaration const *decl) const {
    return data().constants_.get_constant(decl);
  }

  // TODO We probably don't need these. There are likely better ways to expose
  // the requisite information.
  DependentComputedData const &data() const {
    notification_.WaitForNotification();
    return data_;
  }
  DependentComputedData &data() { return data_; }

 protected:
  // Child classes must call this when compilation of this module is complete
  // to notify other modules which may be waiting on data for their own
  // compilation.
  void CompilationComplete() { notification_.Notify(); }

 private:
  DependentComputedData data_;
  absl::Notification notification_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
