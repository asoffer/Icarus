#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include <cstring>
#include "ir/val.h"
#include "type/type.h"
#include "type/callable.h"

#ifdef ICARUS_USE_LLVM
namespace llvm {
class FunctionType;
}  // namespace llvm
#endif  // ICARUS_USE_LLVM

namespace type {
struct GenericFunction : public Callable {
  GenericFunction() {}
  ~GenericFunction() override {}
  char *WriteTo(char *buf) const override { 
    return std::strcpy(buf, "generic") + string_size();
  }
  size_t string_size() const override { return sizeof("generic") - 1; }
  void EmitAssign(const Type *from_type, ir::Val const &from, ir::Register to,
                  Context *ctx) const override {}
  void EmitInit(ir::Register reg, Context *ctx) const override {}
  void EmitDestroy(ir::Register reg, Context *ctx) const override {}
  ir::Val PrepareArgument(const Type *t, const ir::Val &val,
                          Context *ctx) const override {
    NOT_YET();
  }
  void EmitRepr(ir::Val const &id_val, Context *ctx) const override {}
  Cmp Comparator() const override;
};

struct Function : public Callable {
  TYPE_FNS(Function);
  Function(base::vector<const Type *> in, base::vector<const Type *> out)
      : input(std::move(in)), output(std::move(out)) {
    for (auto *t : input) { ASSERT(t != nullptr); }
    for (auto *t : output) { ASSERT(t != nullptr); }
  }

#ifdef ICARUS_USE_LLVM
  llvm::FunctionType *llvm_fn(llvm::LLVMContext &ctx) const;
#endif  // ICARUS_USE_LLVM

  base::vector<const Type *> input, output;
};

Function const *Func(base::vector<Type const *> in,
                     base::vector<Type const *> out);

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_H
