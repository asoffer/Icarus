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
  explicit Opaque(module::BasicModule const *mod)
      : LegacyType(IndexOf<Opaque>(),
                   LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 0,
                                     .is_movable               = 0,
                                     .has_destructor           = 0}),
        mod_(mod) {}

  void WriteTo(std::string *result) const override {
    result->append("<opaque>");
  }

  Completeness completeness() const override {
    return Completeness::Incomplete;
  }

  core::Bytes bytes(core::Arch const &arch) const override {
    UNREACHABLE("Must not request the size of an opaque type");
  }

  core::Alignment alignment(core::Arch const &arch) const override {
    UNREACHABLE("Must not request the alignment of an opaque type");
  }

  bool IsDefaultInitializable() const { UNREACHABLE(); }

  module::BasicModule const *defining_module() const { return mod_; }

 private:
  module::BasicModule const *mod_;
};

struct OpaqueTypeInstruction
    : base::Extend<OpaqueTypeInstruction>::With<base::BaseSerializeExtension,
                                                base::BaseTraverseExtension,
                                                ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = opaque %1$s";

  Type Resolve() const { return type::Allocate<type::Opaque>(mod); }

  module::BasicModule const *mod;
  ir::Reg result;
};

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
