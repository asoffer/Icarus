#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include "type.h"

namespace llvm {
class FunctionType;
}  // namespace llvm

namespace type {
struct Function : public Type {
  TYPE_FNS(Function);
  Function(std::vector<const Type *> in, std::vector<const Type *> out)
      : input(std::move(in)), output(std::move(out)) {
    for (auto *t : input) { ASSERT_NOT_NULL(t); }
    for (auto *t : output) { ASSERT_NOT_NULL(t); }
  }

  llvm::FunctionType *llvm_fn(llvm::LLVMContext &ctx) const;

  const Function* ToIR() const;
  std::vector<const Type *> input, output;
};

const Function *Func(const Type *in, const Type *out);
const Function *Func(std::vector<const Type *> in, const Type *out);
const Function *Func(const Type *in, std::vector<Type *> out);
const Function *Func(std::vector<const Type *> in,
                     std::vector<const Type *> out);

} // namespace type

#endif // ICARUS_TYPE_FUNCTION_H
