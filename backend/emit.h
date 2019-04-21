#ifdef ICARUS_USE_LLVM
#ifndef ICARUS_BACKEND_EMIT_H
#define ICARUS_BACKEND_EMIT_H

#include <memory>
#include <vector>

namespace llvm {
class Module;
}  // namespace llvm

namespace backend {
void EmitAll(std::vector<std::unique_ptr<ir::CompiledFn>> const &fns,
             llvm::Module *module);

}  // namespace backend

#endif  // ICARUS_BACKEND_EMIT_H
#endif  // ICARUS_USE_LLVM
