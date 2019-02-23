#ifndef ICARUS_TYPE_INTERFACE_H
#define ICARUS_TYPE_INTERFACE_H

#include "misc/scope.h"
#include "type/type.h"

struct Architecture;

namespace type {
struct Interface : public Type {
  Interface() = delete;
  ~Interface() {}
  Interface(::Scope const *scope, ::Module const *mod)
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

  virtual Cmp Comparator() const { UNREACHABLE(); }

  ::Module const *defining_module() const { return mod_; }

  virtual void defining_modules(
      std::unordered_set<::Module const *> *modules) const;

#ifdef ICARUS_USE_LLVM
  virtual llvm::Type *llvm(llvm::LLVMContext &) const { UNREACHABLE(); }
#endif  // ICARUS_USE_LLVM

  ::Scope const *scope_ = nullptr;
  ::Module const *mod_  = nullptr;
};

}  // namespace type

#endif  // ICARUS_TYPE_INTERFACE_H
