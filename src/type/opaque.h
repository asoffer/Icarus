#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "base/debug.h"
#include "ir/val.h"
#include "type/type.h"

namespace type {
struct Opaque : public Type {
  ~Opaque() override {}
  char *WriteTo(char *buf) const override;
  size_t string_size() const override;
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
    NOT_YET();
  }

  Cmp Comparator() const override { UNREACHABLE(); }
};

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
