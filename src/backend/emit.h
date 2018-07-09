#ifdef ICARUS_USE_LLVM
#ifndef ICARUS_BACKEND_EMIT_H
#define ICARUS_BACKEND_EMIT_H

#include "base/container/vector.h"
#include <memory>

namespace llvm {
class Module;
}  // namespace llvm

namespace IR {
struct Func;
}  // namespace IR

namespace backend {
void EmitAll(const base::vector<std::unique_ptr<IR::Func>> &fns,
             llvm::Module *module);

}  // namespace backend

#endif  // ICARUS_BACKEND_EMIT_H
#endif  // ICARUS_USE_LLVM
