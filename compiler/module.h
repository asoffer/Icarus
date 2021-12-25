#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include <memory>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "absl/synchronization/notification.h"
#include "ast/ast_fwd.h"
#include "compiler/context.h"
#include "ir/compiled_fn.h"
#include "ir/module.h"
#include "module/module.h"

namespace compiler {

struct CompiledModule : module::BasicModule {
  explicit CompiledModule(frontend::SourceBuffer const *buffer,
                          Context *context = nullptr)
      : module::BasicModule(buffer), context_(ASSERT_NOT_NULL(context)) {}
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
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
