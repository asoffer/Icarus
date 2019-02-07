#ifdef ICARUS_USE_LLVM
#ifndef ICARUS_BACKEND_EMIT_H
#define ICARUS_BACKEND_EMIT_H

#include <memory>
#include "base/container/vector.h"

namespace llvm {
class Module;
}  // namespace llvm

namespace ir {
struct Func;
}  // namespace ir

namespace backend {
void EmitAll(base::vector<std::unique_ptr<ir::Func>> const &fns,
             llvm::Module *module);

}  // namespace backend

#endif  // ICARUS_BACKEND_EMIT_H
#endif  // ICARUS_USE_LLVM
