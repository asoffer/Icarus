#ifndef ICARUS_IR_INSTRUCTION_ARITHMETIC_H
#define ICARUS_IR_INSTRUCTION_ARITHMETIC_H

#include <string_view>

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/interpreter.h"

namespace ir {

// TODO: Support all flavors of overflow behavior (undefined, trap, saturate,
// wrap). Note: We rely on these instructions being templates with only type
// parameters, so when we add overflow support, we must use type tags rather
// than enums.

template <typename NumType>
struct AddInstruction
    : base::Extend<AddInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = add %1$s %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   AddInstruction const &inst) {
    if constexpr (interpreter::FitsInRegister<num_type>) {
      interpreter.frame().set(inst.result,
                              interpreter.frame().resolve(inst.lhs) +
                                  interpreter.frame().resolve(inst.rhs));
    } else {
      new (interpreter.frame().resolve<addr_t>(inst.result))
          num_type(*reinterpret_cast<num_type const *>(
                       interpreter.frame().resolve(inst.lhs)) +
                   *reinterpret_cast<num_type const *>(
                       interpreter.frame().resolve(inst.rhs)));
    }
    return true;
  }

  RegOr<operand_type> lhs;
  RegOr<operand_type> rhs;
  Reg result;
};

template <typename NumType>
struct SubInstruction
    : base::Extend<SubInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = sub %1$s %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   SubInstruction const &inst) {
    if constexpr (interpreter::FitsInRegister<num_type>) {
      interpreter.frame().set(inst.result,
                              interpreter.frame().resolve(inst.lhs) -
                                  interpreter.frame().resolve(inst.rhs));
    } else {
      new (interpreter.frame().resolve<addr_t>(inst.result))
          num_type(*reinterpret_cast<num_type const *>(
                       interpreter.frame().resolve(inst.lhs)) -
                   *reinterpret_cast<num_type const *>(
                       interpreter.frame().resolve(inst.rhs)));
    }
    return true;
  }

  RegOr<operand_type> lhs;
  RegOr<operand_type> rhs;
  Reg result;
};

template <typename NumType>
struct MulInstruction
    : base::Extend<MulInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = mul %1$s %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   MulInstruction const &inst) {
    if constexpr (interpreter::FitsInRegister<num_type>) {
      interpreter.frame().set(inst.result,
                              interpreter.frame().resolve(inst.lhs) *
                                  interpreter.frame().resolve(inst.rhs));
    } else {
      new (interpreter.frame().resolve<addr_t>(inst.result))
          num_type(*reinterpret_cast<num_type const *>(
                       interpreter.frame().resolve(inst.lhs)) *
                   *reinterpret_cast<num_type const *>(
                       interpreter.frame().resolve(inst.rhs)));
    }
    return true;
  }

  RegOr<operand_type> lhs;
  RegOr<operand_type> rhs;
  Reg result;
};

template <typename NumType>
struct DivInstruction
    : base::Extend<DivInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = div %1$s %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   DivInstruction const &inst) {
    if constexpr (interpreter::FitsInRegister<num_type>) {
      auto denominator = interpreter.frame().resolve(inst.rhs);
      if (denominator == 0) {
        interpreter.FatalError("Division by zero.");
        return false;
      }
      interpreter.frame().set(
          inst.result, interpreter.frame().resolve(inst.lhs) / denominator);
    } else {
      auto const &denominator = *reinterpret_cast<num_type const *>(
          interpreter.frame().resolve(inst.rhs));

      if (denominator == 0) {
        interpreter.FatalError("Division by zero.");
        return false;
      }

      new (interpreter.frame().resolve<addr_t>(inst.result))
          num_type(*reinterpret_cast<num_type const *>(
                       interpreter.frame().resolve(inst.lhs)) /
                   denominator);
    }
    return true;
  }

  RegOr<operand_type> lhs;
  RegOr<operand_type> rhs;
  Reg result;
};

template <typename NumType>
struct ModInstruction
    : base::Extend<ModInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = mod %1$s %2$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   ModInstruction const &inst) {
    if constexpr (interpreter::FitsInRegister<num_type>) {
      interpreter.frame().set(inst.result,
                              interpreter.frame().resolve(inst.lhs) %
                                  interpreter.frame().resolve(inst.rhs));
    } else {
      new (interpreter.frame().resolve<addr_t>(inst.result))
          num_type(*reinterpret_cast<num_type const *>(
                       interpreter.frame().resolve(inst.lhs)) %
                   *reinterpret_cast<num_type const *>(
                       interpreter.frame().resolve(inst.rhs)));
    }
    return true;
  }

  RegOr<operand_type> lhs;
  RegOr<operand_type> rhs;
  Reg result;
};

template <typename NumType>
struct NegInstruction
    : base::Extend<NegInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type     = NumType;
  using operand_type = std::conditional_t<interpreter::FitsInRegister<num_type>,
                                          num_type, addr_t>;
  static constexpr std::string_view kDebugFormat = "%2$s = neg %1$s";

  friend bool InterpretInstruction(interpreter::Interpreter &interpreter,
                                   NegInstruction const &inst) {
    if constexpr (interpreter::FitsInRegister<num_type>) {
      interpreter.frame().set(inst.result,
                              -interpreter.frame().resolve(inst.operand));
    } else {
      new (interpreter.frame().resolve<addr_t>(inst.result))
          num_type(-*reinterpret_cast<num_type const *>(
              interpreter.frame().resolve(inst.operand)));
    }
    return true;
  }

  RegOr<operand_type> operand;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_ARITHMETIC_H
