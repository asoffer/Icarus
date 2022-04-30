#ifndef ICARUS_TYPE_POINTER_INSTRUCTIONS_H
#define ICARUS_TYPE_POINTER_INSTRUCTIONS_H

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/base.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/interpreter.h"
#include "ir/interpreter/stack_frame.h"
#include "type/type.h"

namespace type {

struct PtrInstruction
    : base::Extend<PtrInstruction>::With<base::BaseSerializeExtension,
                                         base::BaseTraverseExtension,
                                         ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = ptr %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                                   PtrInstruction const &inst) {
    interpreter.frame().set(
        inst.result, Type(Ptr(interpreter.frame().resolve(inst.operand))));
    return true;
  }

  Type Resolve() const { return Apply(operand.value()); }
  static type::Type Apply(type::Type operand) { return type::Ptr(operand); }

  ir::RegOr<type::Type> operand;
  ir::Reg result;
};

struct BufPtrInstruction
    : base::Extend<BufPtrInstruction>::With<base::BaseSerializeExtension,
                                            base::BaseTraverseExtension,
                                            ir::DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = buf-ptr %1$s";

  friend bool InterpretInstruction(ir::interpreter::Interpreter &interpreter,
                                   BufPtrInstruction const &inst) {
    interpreter.frame().set(
        inst.result, Type(BufPtr(interpreter.frame().resolve(inst.operand))));
    return true;
  }

  Type Resolve() const { return Apply(operand.value()); }
  static type::Type Apply(type::Type operand) { return type::BufPtr(operand); }

  ir::RegOr<type::Type> operand;
  ir::Reg result;
};

}  // namespace type

#endif  // ICARUS_TYPE_POINTER_INSTRUCTIONS_H
