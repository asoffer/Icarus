#ifndef ICARUS_COMPILER_LIBRARY_MODULE_H
#define ICARUS_COMPILER_LIBRARY_MODULE_H

#include "compiler/module.h"

namespace compiler {
struct LibraryModule : CompiledModule {
  template <typename ProcessFn,
            typename std::enable_if_t<
                not std::is_same_v<ProcessFn, CompiledModule>, int> = 0>
  explicit LibraryModule(ProcessFn fn) : CompiledModule(std::move(fn)) {}
};

}  // namespace compiler

#endif // ICARUS_COMPILER_LIBRARY_MODULE_H
