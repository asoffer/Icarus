#ifndef ICARUS_TYPE_GENERIC_H
#define ICARUS_TYPE_GENERIC_H

#include <string>
#include <vector>

#include "core/arch.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/interpreter.h"
#include "ir/value/interface.h"
#include "type/type.h"

namespace type {

struct Generic : LegacyType {
  explicit Generic(ir::Interface intf)
      : LegacyType(IndexOf<Generic>(),
                   LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 0,
                                     .is_movable               = 0,
                                     .has_destructor           = 0}),
        interface_(intf) {}

  void WriteTo(std::string *result) const override {
    result->append("generic");
  }

  bool is_big() const override { return false; }

  Completeness completeness() const override { return Completeness::Complete; }
  core::Bytes bytes(core::Arch const &) const override { UNREACHABLE(); }
  core::Alignment alignment(core::Arch const &) const override {
    UNREACHABLE();
  }

  ir::Interface const &interface() const { return interface_; }

 private:
  ir::Interface interface_;
};

struct GenericTypeInstruction 
    : base::Extend<GenericTypeInstruction>::With<base::BaseSerializeExtension,
                                           base::BaseTraverseExtension,
                                           ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = generic-type %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                                   GenericTypeInstruction const &inst) {
    interpreter.frame().set(
        inst.result,
        Type(new Generic(interpreter.frame().resolve(inst.interface))));
    return true;
  }

  ir::RegOr<ir::Interface> interface;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_H
