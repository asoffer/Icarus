#ifndef ICARUS_TYPE_FUNCTION_H
#define ICARUS_TYPE_FUNCTION_H

#include <cstring>

#include "core/fn_params.h"
#include "type/callable.h"
#include "type/typed_value.h"

#ifdef ICARUS_USE_LLVM
namespace llvm {
class FunctionType;
}  // namespace llvm
#endif  // ICARUS_USE_LLVM

namespace type {
struct GenericFunction : public Callable {
  GenericFunction() {}
  ~GenericFunction() override {}
  void WriteTo(std::string *result) const override {
    result->append("generic");
  }

#include "visitor/type_visitors.xmacro.h"

  ir::Results PrepareArgument(const Type *t, const ir::Results &val,
                          Context *ctx) const override;
  void EmitRepr(ir::Results const &id_val, Context *ctx) const override;
  void defining_modules(
      absl::flat_hash_set<::Module const *> *modules) const override;

  bool ReinterpretAs(Type const *t) const override;

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  Cmp Comparator() const override;
};

struct Function : public Callable {
  TYPE_FNS(Function);
  Function(std::vector<const Type *> in, std::vector<const Type *> out)
      : input(std::move(in)), output(std::move(out)) {
    for (auto *t : input) { ASSERT(t != nullptr); }
    for (auto *t : output) { ASSERT(t != nullptr); }
  }

#include "visitor/type_visitors.xmacro.h"

  core::FnParams<type::Typed<ast::Expression const *>> AnonymousFnParams()
      const;

#ifdef ICARUS_USE_LLVM
  llvm::FunctionType *llvm_fn(llvm::LLVMContext &ctx) const;
#endif  // ICARUS_USE_LLVM

  std::vector<const Type *> input, output;
};

Function const *Func(std::vector<Type const *> in,
                     std::vector<Type const *> out);

}  // namespace type

#endif  // ICARUS_TYPE_FUNCTION_H
