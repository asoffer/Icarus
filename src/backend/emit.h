#ifndef ICARUS_BACKEND_EMIT_H
#define ICARUS_BACKEND_EMIT_H

namespace llvm {
class Module;
} // namespace llvm

namespace IR {
struct Func;
} // namespace IR

namespace backend {
void Emit(const IR::Func &fn, llvm::Module *module);
} // namespace backend

#endif // ICARUS_BACKEND_EMIT_H
