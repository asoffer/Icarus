#ifndef ICARUS_COMPILER_EXECUTABLE_MODULE_H
#define ICARUS_COMPILER_EXECUTABLE_MODULE_H

#include "compiler/module.h"
#include "ir/compiled_fn.h"

namespace compiler {

struct ExecutableModule : CompiledModule {
  template <typename ProcessFn,
            typename std::enable_if_t<
                not std::is_same_v<ProcessFn, CompiledModule>, int> = 0>
  explicit ExecutableModule(ProcessFn fn) : CompiledModule(std::move(fn)) {}

  ir::CompiledFn *main() { return &main_; }

  // TODO hide this
  void set_main(ir::CompiledFn *main_fn) {}

 private:
  ir::CompiledFn main_ = ir::CompiledFn(type::Func({}, {}), {});
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_EXECUTABLE_MODULE_H
