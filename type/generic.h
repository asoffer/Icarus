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
  void WriteTo(std::string *result) const override {
    result->append(manager_->DebugString(interface_));
  }

  bool is_big() const override { return false; }

  Completeness completeness() const override { return Completeness::Complete; }
  core::Bytes bytes(core::Arch const &) const override { UNREACHABLE(); }
  core::Alignment alignment(core::Arch const &) const override {
    UNREACHABLE();
  }

  ir::Interface const &interface() const { return interface_; }
  ir::InterfaceManager &manager() const { return *manager_; }

  friend bool operator==(Generic const &lhs, Generic const &rhs) {
    return lhs.interface_ == rhs.interface_ and lhs.manager_ == rhs.manager_;
  }

  friend bool operator!=(Generic const &lhs, Generic const &rhs) {
    return not (lhs == rhs);
  }

  template <typename H>
  friend H AbslHashValue(H h, Generic const &g) {
    return H::combine(std::move(h), g.interface_, g.manager_);
  }

 private:
  friend Generic const *Gen(ir::Interface interface,
                            ir::InterfaceManager *manager);

  friend struct GenericTypeInstruction;

  explicit Generic(ir::Interface intf, ir::InterfaceManager *manager)
      : LegacyType(IndexOf<Generic>(),
                   LegacyType::Flags{.is_default_initializable = 0,
                                     .is_copyable              = 0,
                                     .is_movable               = 0,
                                     .has_destructor           = 0}),
        interface_(intf),
        manager_(ASSERT_NOT_NULL(manager)) {}

  ir::Interface interface_;
  ir::InterfaceManager *manager_;
};

Generic const *Gen(ir::Interface interface, ir::InterfaceManager *manager);

struct GenericTypeInstruction
    : base::Extend<GenericTypeInstruction>::With<base::BaseSerializeExtension,
                                                 base::BaseTraverseExtension,
                                                 ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = generic-type %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                                   GenericTypeInstruction const &inst) {
    auto &manager = *reinterpret_cast<ir::InterfaceManager *>(
        interpreter.frame().resolve<ir::addr_t>(inst.manager));
    interpreter.frame().set(
        inst.result,
        Type(Gen(interpreter.frame().resolve(inst.interface), &manager)));
    return true;
  }

  ir::RegOr<ir::Interface> interface;
  ir::Reg manager;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_GENERIC_H
