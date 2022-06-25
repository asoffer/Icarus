#ifndef ICARUS_COMPILER_INTERFACE_INSTRUCTIONS_H
#define ICARUS_COMPILER_INTERFACE_INSTRUCTIONS_H

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "compiler/resources.h"
#include "core/arguments.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/interpreter.h"
#include "ir/value/interface.h"
#include "ir/value/reg.h"
#include "type/type.h"

namespace compiler {

struct LoadInterfaceManagerInstruction
    : base::Extend<LoadInterfaceManagerInstruction>::With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%1$s = interface-manager";

  friend bool InterpretInstruction(
      ir::interpreter::Interpreter &interpreter,
      LoadInterfaceManagerInstruction const &inst) {
    interpreter.frame().set(inst.result, &compiler::GlobalInterfaceManager);
    return true;
  }

  ir::Reg result;
};

struct PointerInterfaceInstruction
    : base::Extend<PointerInterfaceInstruction>::With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%3$s = ptr-intf %2$s (%1$s)";

  friend bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                                   PointerInterfaceInstruction const &inst) {
    auto &im = *reinterpret_cast<ir::InterfaceManager *>(
        interpreter.frame().resolve<ir::addr_t>(inst.manager));
    interpreter.frame().set(
        inst.result, im.Pointer(interpreter.frame().resolve(inst.pointee)));
    return true;
  }

  ir::Reg manager;
  ir::RegOr<ir::Interface> pointee;
  ir::Reg result;
};

struct CastTypeToInterface
    : base::Extend<CastTypeToInterface>::With<base::BaseTraverseExtension,
                                              base::BaseSerializeExtension,
                                              ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%3$s = precisely %2$s (%1$s)";

  friend bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                                   CastTypeToInterface const &inst) {
    auto &im = *reinterpret_cast<ir::InterfaceManager *>(
        interpreter.frame().resolve<ir::addr_t>(inst.manager));
    interpreter.frame().set(
        inst.result, im.Precisely(interpreter.frame().resolve(inst.type)));
    return true;
  }

  ir::Reg manager;
  ir::RegOr<type::Type> type;
  ir::Reg result;
};

struct CallableInterfaceInstruction {
  std::string to_string() const {
    std::string args;
    std::string_view separator;
    for (auto const &i : arguments.pos()) {
      absl::StrAppend(&args, std::exchange(separator, ", "),
                      base::UniversalPrintToString(i));
    }
    for (auto const &[name, i] : arguments.named()) {
      absl::StrAppend(&args, std::exchange(separator, ", "), name, " = ",
                      base::UniversalPrintToString(i));
    }
    return absl::StrFormat("%s = callable [%s] (%s)",
                           base::UniversalPrintToString(result), args,
                           base::UniversalPrintToString(manager));
  }

  friend bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                                   CallableInterfaceInstruction const &inst) {
    core::Arguments<ir::Interface> arguments;
    for (auto const &value : inst.arguments.pos()) {
      arguments.pos_emplace(interpreter.frame().resolve(value));
    }
    for (auto const &[name, value] : inst.arguments.named()) {
      arguments.named_emplace(name, interpreter.frame().resolve(value));
    }
    auto &im = *reinterpret_cast<ir::InterfaceManager *>(
        interpreter.frame().resolve<ir::addr_t>(inst.manager));
    interpreter.frame().set(inst.result, ir::Interface(im.Callable(arguments)));
    return true;
  }

  friend void BaseSerialize(base::Serializer auto &s,
                            CallableInterfaceInstruction const &inst) {
    base::Serialize(s, inst.manager, inst.arguments.pos(),
                    inst.arguments.named(), inst.result);
  }

  friend bool BaseDeserialize(base::Deserializer auto &d,
                              CallableInterfaceInstruction &inst) {
    std::vector<ir::RegOr<ir::Interface>> pos;
    absl::flat_hash_map<std::string, ir::RegOr<ir::Interface>> named;
    if (not base::Deserialize(d, inst.manager, pos, named, inst.result)) {
      return false;
    }
    inst.arguments = core::Arguments<ir::RegOr<ir::Interface>>(
        std::move(pos), std::move(named));
    return true;
  }

  friend void BaseTraverse(ir::Inliner &inliner,
                           CallableInterfaceInstruction &inst) {
    base::Traverse(inliner, inst.arguments.pos(), inst.arguments.named(),
                   inst.result);
  }

  ir::Reg manager;
  core::Arguments<ir::RegOr<ir::Interface>> arguments;
  ir::Reg result;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_INTERFACE_INSTRUCTIONS_H
