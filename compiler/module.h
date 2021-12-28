#ifndef ICARUS_COMPILER_MODULE_H
#define ICARUS_COMPILER_MODULE_H

#include "compiler/context.h"
#include "module/module.h"

namespace compiler {

struct CompiledModule : module::BasicModule {
  explicit CompiledModule(frontend::SourceBuffer const *buffer,
                          Context *context = nullptr)
      : module::BasicModule(buffer), context_(ASSERT_NOT_NULL(context)) {}
  ~CompiledModule() override {}

  Context const &context() const { return *context_; }
  Context &context() { return *context_; }

 private:
  Context *context_;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_MODULE_H
