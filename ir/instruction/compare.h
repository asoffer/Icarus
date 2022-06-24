#ifndef ICARUS_IR_INSTRUCTION_COMPARE_H
#define ICARUS_IR_INSTRUCTION_COMPARE_H

#include <string_view>

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/interpreter.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace ir {

template <typename NumType>
struct EqInstruction : base::Extend<EqInstruction<NumType>>::template With<
                           base::BaseTraverseExtension,
                           base::BaseSerializeExtension, DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = eq %1$s %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   EqInstruction<num_type> const &inst) {
    bool value;
    if constexpr (interpreter::FitsInRegister<num_type>) {
      value = (interpreter.frame().resolve(inst.lhs) ==
               interpreter.frame().resolve(inst.rhs));
    } else {
      value = (*reinterpret_cast<num_type const *>(
                   interpreter.frame().resolve(inst.lhs)) ==
               *reinterpret_cast<num_type const *>(
                   interpreter.frame().resolve(inst.rhs)));
    }

    interpreter.frame().set(inst.result, value);
    return true;
  }

  RegOr<operand_type> lhs;
  RegOr<operand_type> rhs;
  Reg result;
};

template <typename NumType>
struct NeInstruction : base::Extend<NeInstruction<NumType>>::template With<
                           base::BaseTraverseExtension,
                           base::BaseSerializeExtension, DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = ne %1$s %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   NeInstruction<num_type> const &inst) {
    bool value;
    if constexpr (interpreter::FitsInRegister<num_type>) {
      value = (interpreter.frame().resolve(inst.lhs) !=
               interpreter.frame().resolve(inst.rhs));
    } else {
      value = (*reinterpret_cast<num_type const *>(
                   interpreter.frame().resolve(inst.lhs)) !=
               *reinterpret_cast<num_type const *>(
                   interpreter.frame().resolve(inst.rhs)));
    }

    interpreter.frame().set(inst.result, value);
    return true;
  }

  RegOr<operand_type> lhs;
  RegOr<operand_type> rhs;
  Reg result;
};

template <typename NumType>
struct LtInstruction : base::Extend<LtInstruction<NumType>>::template With<
                           base::BaseTraverseExtension,
                           base::BaseSerializeExtension, DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = lt %1$s %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   LtInstruction<num_type> const &inst) {
    bool value;
    if constexpr (interpreter::FitsInRegister<num_type>) {
      value = (interpreter.frame().resolve(inst.lhs) <
               interpreter.frame().resolve(inst.rhs));
    } else {
      value = (*reinterpret_cast<num_type const *>(
                   interpreter.frame().resolve(inst.lhs)) <
               *reinterpret_cast<num_type const *>(
                   interpreter.frame().resolve(inst.rhs)));
    }

    interpreter.frame().set(inst.result, value);
    return true;
  }

  RegOr<operand_type> lhs;
  RegOr<operand_type> rhs;
  Reg result;
};

template <typename NumType>
struct LeInstruction : base::Extend<LeInstruction<NumType>>::template With<
                           base::BaseTraverseExtension,
                           base::BaseSerializeExtension, DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = le %1$s %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   LeInstruction<num_type> const &inst) {
    bool value;
    if constexpr (interpreter::FitsInRegister<num_type>) {
      value = (interpreter.frame().resolve(inst.lhs) <=
               interpreter.frame().resolve(inst.rhs));
    } else {
      value = (*reinterpret_cast<num_type const *>(
                   interpreter.frame().resolve(inst.lhs)) <=
               *reinterpret_cast<num_type const *>(
                   interpreter.frame().resolve(inst.rhs)));
    }

    interpreter.frame().set(inst.result, value);
    return true;
  }

  RegOr<operand_type> lhs;
  RegOr<operand_type> rhs;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_COMPARE_H
