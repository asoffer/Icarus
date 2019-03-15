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

  void WriteTo(std::string *result) const override;

  void EmitCopyAssign(Type const *from_type, ir::Results const &from,
                      ir::RegisterOr<ir::Addr> to, Context *ctx) const override;
  void EmitMoveAssign(Type const *from_type, ir::Results const &from,
                      ir::RegisterOr<ir::Addr> to, Context *ctx) const override;

  void EmitInit(ir::Register reg, Context *ctx) const override {
    UNREACHABLE();
  }

  void EmitDestroy(ir::Register reg, Context *ctx) const override {
    UNREACHABLE();
  }

  ir::Results PrepareArgument(Type const *t, ir::Results const &val,
                              Context *ctx) const override;

  void EmitRepr(ir::Results const &id_val, Context *ctx) const override;

  bool matches(Type const *t) const;

  layout::Bytes bytes(layout::Arch const &arch) const override;
  layout::Alignment alignment(layout::Arch const &arch) const override;

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
