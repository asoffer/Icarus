#ifndef ICARUS_COMPILER_BUILTIN_MODULE_H
#define ICARUS_COMPILER_BUILTIN_MODULE_H

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/set.h"
#include "ir/interpreter/interpreter.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/scope_context.h"
#include "ir/value/slice.h"
#include "module/builtin.h"
#include "type/type.h"

namespace compiler {

struct AbortInstruction
    : base::Extend<AbortInstruction>::With<base::BaseTraverseExtension,
                                           base::BaseSerializeExtension,
                                           ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "abort";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   AbortInstruction const&) {
    interpreter.FatalError("`builtin.abort` invoked.");
    return false;
  }
};

struct AlignmentInstruction
    : base::Extend<AlignmentInstruction>::With<base::BaseTraverseExtension,
                                               base::BaseSerializeExtension,
                                               ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = alignment %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   AlignmentInstruction const& inst) {
    interpreter.frame().set(
        inst.result,
        interpreter.frame().resolve(inst.type).alignment(core::Host).value());
    return true;
  }

  ir::RegOr<type::Type> type;
  ir::Reg result;
};

struct AsciiEncodeInstruction
    : base::Extend<AsciiEncodeInstruction>::With<base::BaseTraverseExtension,
                                                 base::BaseSerializeExtension,
                                                 ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = ascii-encode %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   AsciiEncodeInstruction const& inst) {
    interpreter.frame().set(inst.result,
                            ir::Char(interpreter.frame().resolve(inst.value)));
    return true;
  }

  ir::RegOr<uint8_t> value;
  ir::Reg result;
};

struct AsciiDecodeInstruction
    : base::Extend<AsciiDecodeInstruction>::With<base::BaseTraverseExtension,
                                                 base::BaseSerializeExtension,
                                                 ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = ascii-decode %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   AsciiDecodeInstruction const& inst) {
    interpreter.frame().set(
        inst.result,
        static_cast<uint8_t>(interpreter.frame().resolve(inst.character)));
    return true;
  }

  ir::RegOr<ir::Char> character;
  ir::Reg result;
};

struct BytesInstruction
    : base::Extend<BytesInstruction>::With<base::BaseTraverseExtension,
                                           base::BaseSerializeExtension,
                                           ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = bytes %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   BytesInstruction const& inst) {
    interpreter.frame().set(
        inst.result,
        interpreter.frame().resolve(inst.type).bytes(core::Host).value());
    return true;
  }

  ir::RegOr<type::Type> type;
  ir::Reg result;
};

struct HasBlockInstruction
    : base::Extend<HasBlockInstruction>::With<base::BaseTraverseExtension,
                                              base::BaseSerializeExtension,
                                              ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = has_block %1$s %2$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter& interpreter,
                                   HasBlockInstruction const& inst) {
    interpreter.frame().set(
        inst.result, interpreter.frame()
                             .resolve(inst.context)
                             .find(interpreter.frame().resolve(inst.name)) !=
                         ir::Block::Invalid());
    return true;
  }

  ir::RegOr<ir::ScopeContext> context;
  ir::RegOr<ir::Slice> name;
  ir::Reg result;
};

using BuiltinInstructions =
    ir::InstructionSet<AbortInstruction, AlignmentInstruction,
                       AsciiEncodeInstruction, AsciiDecodeInstruction,
                       BytesInstruction, HasBlockInstruction>;

// Returns a BuiltinModule consisting of all nodes built in as language
// intrinsics.
std::unique_ptr<module::BuiltinModule> MakeBuiltinModule();

}  // namespace compiler

#endif  // ICARUS_COMPILER_BUILTIN_MODULE_H
