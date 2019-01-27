#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "base/debug.h"
#include "ir/val.h"
#include "type/type.h"

struct Module;

namespace type {
struct Opaque : public Type {
  Opaque(::Module const *mod) : mod_(mod) {}
  ~Opaque() override {}
  void WriteTo(std::string *result) const { result->append("<opaque>"); }
  void EmitAssign(const Type *from_type, ir::Val const &from,
                  ir::RegisterOr<ir::Addr> to, Context *ctx) const override {
    UNREACHABLE();
  }
  void EmitInit(ir::Register reg, Context *ctx) const override {
    UNREACHABLE();
  }
  void EmitDestroy(ir::Register reg, Context *ctx) const override {
    UNREACHABLE();
  }
  ir::Val PrepareArgument(const Type *t, const ir::Val &val,
                          Context *ctx) const override {
    UNREACHABLE();
  }
  void EmitRepr(ir::Val const &id_val, Context *ctx) const override {
    UNREACHABLE();
  }

  void defining_modules(
      std::unordered_set<::Module const *> *modules) const override {
    modules->insert(mod_);
  }

  bool IsDefaultInitializable() const override { return false; }

  Cmp Comparator() const override { UNREACHABLE(); }
  ::Module const *mod_;
};

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
