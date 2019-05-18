#ifndef ICARUS_TYPE_INTERFACE_H
#define ICARUS_TYPE_INTERFACE_H

#include "core/scope.h"
#include "type/type.h"

namespace type {
struct Interface : public Type {
  Interface() = delete;
  ~Interface() {}
  Interface(core::Scope const *scope, ::Module const *mod)
      : scope_(scope), mod_(mod) {}

#include "visitor/type_visitors.xmacro.h"

  void WriteTo(std::string *result) const override;

  ir::Results PrepareArgument(Type const *t, ir::Results const &val,
                              Context *ctx) const override;

  void EmitRepr(ir::Results const &id_val, Context *ctx) const override;

  bool matches(Type const *t) const;

  bool ReinterpretAs(Type const *t) const override;

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

   Cmp Comparator() const override { UNREACHABLE(); }

  ::Module const *defining_module() const { return mod_; }

  void defining_modules(
      absl::flat_hash_set<::Module const *> *modules) const override;

#ifdef ICARUS_USE_LLVM
  llvm::Type *llvm(llvm::LLVMContext &) const override { UNREACHABLE(); }
#endif  // ICARUS_USE_LLVM

  core::Scope const *scope_ = nullptr;
  ::Module const *mod_  = nullptr;
};

}  // namespace type

#endif  // ICARUS_TYPE_INTERFACE_H
