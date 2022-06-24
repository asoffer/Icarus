#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "base/debug.h"
#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/value/module_id.h"
#include "type/type.h"

namespace type {
struct Opaque : LegacyType {
  explicit Opaque(ir::ModuleId mod);

  void WriteTo(std::string *result) const override;

  Completeness completeness() const override {
    return Completeness::Incomplete;
  }

  core::Bytes bytes(core::Arch const &arch) const override;
  core::Alignment alignment(core::Arch const &arch) const override;

  bool IsDefaultInitializable() const { UNREACHABLE(); }

  uintptr_t numeric_id() const { return reinterpret_cast<uintptr_t>(this); }

  ir::ModuleId defining_module() const { return mod_; }

 private:
  ir::ModuleId mod_;
};

struct OpaqueTypeInstruction
    : base::Extend<OpaqueTypeInstruction>::With<base::BaseSerializeExtension,
                                                base::BaseTraverseExtension,
                                                ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = opaque %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                                   OpaqueTypeInstruction const &inst);

  Type Resolve() const;

  ir::RegOr<ir::ModuleId> mod;
  ir::Reg result;
};

Opaque const *Opaq(ir::ModuleId mod, uintptr_t numeric_id);

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
