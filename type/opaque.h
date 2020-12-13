#ifndef ICARUS_TYPE_OPAQUE_H
#define ICARUS_TYPE_OPAQUE_H

#include "base/debug.h"
#include "base/extend.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "module/module.h"
#include "type/type.h"

namespace type {
struct Opaque : public LegacyType {
  explicit Opaque(module::BasicModule const *)
      : LegacyType(LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 0,
                                     .is_movable               = 0,
                                     .has_destructor           = 0}) {}

  void Accept(VisitorBase *visitor, void *ret, void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

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
};

struct OpaqueTypeInstruction
    : base::Extend<OpaqueTypeInstruction>::With<ir::ByteCodeExtension,
                                                ir::InlineExtension,
                                                ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = opaque %1$s";

  Type Resolve() const { return type::Allocate<type::Opaque>(mod); }

  module::BasicModule const *mod;
  ir::Reg result;
};

}  // namespace type
#endif  // ICARUS_TYPE_OPAQUE_H
