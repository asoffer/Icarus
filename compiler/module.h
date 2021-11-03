#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include <memory>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "absl/synchronization/notification.h"
#include "ast/ast_fwd.h"
#include "compiler/context.h"
#include "ir/compiled_fn.h"
#include "ir/compiled_jump.h"
#include "ir/module.h"
#include "module/module.h"

namespace compiler {

struct CompiledModule : module::BasicModule {
  explicit CompiledModule(Context *context = nullptr)
      : context_(ASSERT_NOT_NULL(context)), data_(&module_) {}
  ~CompiledModule() override {}

  Context const &context(module::BasicModule const *unused = nullptr) const {
    return *context_;
  }
  Context &context(module::BasicModule const *unused = nullptr) {
    return *context_;
  }

 private:
  ir::Module module_;
  Context *context_;
  Context data_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
