#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "module/module.h"
#include "type/type.h"

namespace type {
struct Opaque : LegacyType {
  explicit Opaque(module::Module const *mod);

  void WriteTo(std::string *result) const override;

  Completeness completeness() const override {
    return Completeness::Incomplete;
  }

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  bool IsDefaultInitializable() const { UNREACHABLE(); }

  uintptr_t numeric_id() const { return reinterpret_cast<uintptr_t>(this); }

  module::Module const *defining_module() const { return mod_; }

 private:
  module::Module const *mod_;
};

struct OpaqueTypeInstruction
    : base::Extend<OpaqueTypeInstruction>::With<base::BaseSerializeExtension,
                                                base::BaseTraverseExtension,
                                                ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = opaque %1$s";

  Type Resolve() const;

  module::Module const *mod;
  ir::Reg result;
};

Opaque const *Opaq(module::Module const *mod, uintptr_t numeric_id);

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
