#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "type/type.h"

struct Module;

namespace type {
struct Opaque : public Type {
  Opaque(::Module const *mod) : mod_(mod) {}
  ~Opaque() override {}
  void WriteTo(std::string *result) const override;
  void EmitCopyAssign(const Type *from_type, ir::Results const &from,
                      ir::RegisterOr<ir::Addr> to, Context *ctx) const override;
  void EmitMoveAssign(const Type *from_type, ir::Results const &from,
                      ir::RegisterOr<ir::Addr> to, Context *ctx) const override;

  void EmitInit(ir::Reg reg, Context *ctx) const override;
  void EmitDestroy(ir::Reg reg, Context *ctx) const override;
  ir::Results PrepareArgument(const Type *t, const ir::Results &val,
                              Context *ctx) const override;
  void EmitRepr(ir::Results const &id_val, Context *ctx) const override;

  bool ReinterpretAs(Type const *t) const override;

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  void defining_modules(
      absl::flat_hash_set<::Module const *> *modules) const override;

  bool IsDefaultInitializable() const override { return false; }

  Cmp Comparator() const override;
  ::Module const *mod_;
};

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
