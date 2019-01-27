#ifndef ICARUS_TYPE_INTERFACE_H
#define ICARUS_TYPE_INTERFACE_H

#include "ir/val.h"  // TODO remove this include
#include "scope.h"
#include "type.h"

struct Architecture;

namespace type {
struct Interface : public Type {
  Interface() = delete;
  ~Interface() {}
  Interface(::Scope const *scope, ::Module const *mod)
      : scope_(scope), mod_(mod) {}

  void WriteTo(std::string *result) const override;

  void EmitCopyAssign(Type const *from_type, ir::Val const &from,
                      ir::RegisterOr<ir::Addr> to, Context *ctx) const override;
  void EmitMoveAssign(Type const *from_type, ir::Val const &from,
                      ir::RegisterOr<ir::Addr> to, Context *ctx) const override;

  virtual void EmitInit(ir::Register reg, Context *ctx) const { UNREACHABLE(); }
  virtual void EmitDestroy(ir::Register reg, Context *ctx) const {
    UNREACHABLE();
  }
  virtual ir::Val PrepareArgument(Type const *t, ir::Val const &val,
                                  Context *ctx) const {
    UNREACHABLE();
  }

  virtual void EmitRepr(ir::Val const &id_val, Context *ctx) const {
    UNREACHABLE();
  }

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
