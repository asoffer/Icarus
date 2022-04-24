#ifndef ICARUS_IR_INSTRUCTION_ARITHMETIC_H
#define ICARUS_IR_INSTRUCTION_ARITHMETIC_H

#include <string_view>

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/debug.h"
#include "ir/interpreter/execution_context.h"
#include "ir/interpreter/legacy_stack_frame.h"

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
  using num_type = NumType;
  using operand_type =
      std::conditional_t<::interpreter::FitsInRegister<num_type>, num_type,
                         addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = add %1$s %2$s";

  friend bool InterpretInstruction(AddInstruction<num_type> const &inst,
                                   interpreter::StackFrame &frame) {
    if constexpr (::interpreter::FitsInRegister<num_type>) {
      frame.set(inst.result, frame.resolve(inst.lhs) + frame.resolve(inst.rhs));
    } else {
      new (frame.resolve<addr_t>(inst.result)) num_type(
          *reinterpret_cast<num_type const *>(frame.resolve(inst.lhs)) +
          *reinterpret_cast<num_type const *>(frame.resolve(inst.rhs)));
    }
    return true;
  }

  void Apply(::interpreter::ExecutionContext &ctx) const
      requires(not ::interpreter::FitsInRegister<num_type>) {
    new (ctx.resolve<addr_t>(result))
        num_type(*reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(lhs)) +
                 *reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(rhs)));
  }

  num_type Resolve() const requires(::interpreter::FitsInRegister<num_type>) {
    return lhs.value() + rhs.value();
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
  using num_type = NumType;
  using operand_type =
      std::conditional_t<::interpreter::FitsInRegister<num_type>, num_type,
                         addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = sub %1$s %2$s";

  friend bool InterpretInstruction(SubInstruction<num_type> const &inst,
                                   interpreter::StackFrame &frame) {
    if constexpr (::interpreter::FitsInRegister<num_type>) {
      frame.set(inst.result, frame.resolve(inst.lhs) - frame.resolve(inst.rhs));
    } else {
      new (frame.resolve<addr_t>(inst.result)) num_type(
          *reinterpret_cast<num_type const *>(frame.resolve(inst.lhs)) -
          *reinterpret_cast<num_type const *>(frame.resolve(inst.rhs)));
    }
    return true;
  }

  void Apply(::interpreter::ExecutionContext &ctx) const
      requires(not ::interpreter::FitsInRegister<num_type>) {
    new (ctx.resolve<addr_t>(result))
        num_type(*reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(lhs)) -
                 *reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(rhs)));
  }

  num_type Resolve() const requires(::interpreter::FitsInRegister<num_type>) {
    return lhs.value() - rhs.value();
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
  using num_type = NumType;
  using operand_type =
      std::conditional_t<::interpreter::FitsInRegister<num_type>, num_type,
                         addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = mul %1$s %2$s";

  friend bool InterpretInstruction(MulInstruction<num_type> const &inst,
                                   interpreter::StackFrame &frame) {
    if constexpr (::interpreter::FitsInRegister<num_type>) {
      frame.set(inst.result, frame.resolve(inst.lhs) * frame.resolve(inst.rhs));
    } else {
      new (frame.resolve<addr_t>(inst.result)) num_type(
          *reinterpret_cast<num_type const *>(frame.resolve(inst.lhs)) *
          *reinterpret_cast<num_type const *>(frame.resolve(inst.rhs)));
    }
    return true;
  }

  void Apply(::interpreter::ExecutionContext &ctx) const
      requires(not ::interpreter::FitsInRegister<num_type>) {
    new (ctx.resolve<addr_t>(result))
        num_type(*reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(lhs)) *
                 *reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(rhs)));
  }

  num_type Resolve() const requires(::interpreter::FitsInRegister<num_type>) {
    return lhs.value() * rhs.value();
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
  using num_type = NumType;
  using operand_type =
      std::conditional_t<::interpreter::FitsInRegister<num_type>, num_type,
                         addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = div %1$s %2$s";

  friend bool InterpretInstruction(DivInstruction<num_type> const &inst,
                                   interpreter::StackFrame &frame) {
    if constexpr (::interpreter::FitsInRegister<num_type>) {
      auto denominator = frame.resolve(inst.rhs);
      if (denominator == 0) {
        frame.FatalError("Division by zero.");
        return false;
      }
      frame.set(inst.result, frame.resolve(inst.lhs) / denominator);
    } else {
      auto const &denominator =
          *reinterpret_cast<num_type const *>(frame.resolve(inst.rhs));

      if (denominator == 0) {
        frame.FatalError("Division by zero.");
        return false;
      }

      new (frame.resolve<addr_t>(inst.result)) num_type(
          *reinterpret_cast<num_type const *>(frame.resolve(inst.lhs)) /
          denominator);
    }
    return true;
  }


  void Apply(::interpreter::ExecutionContext &ctx) const
      requires(not ::interpreter::FitsInRegister<num_type>) {
    new (ctx.resolve<addr_t>(result))
        num_type(*reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(lhs)) /
                 *reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(rhs)));
  }

  num_type Resolve() const requires(::interpreter::FitsInRegister<num_type>) {
    return lhs.value() / rhs.value();
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
  using num_type = NumType;
  using operand_type =
      std::conditional_t<::interpreter::FitsInRegister<num_type>, num_type,
                         addr_t>;
  static constexpr std::string_view kDebugFormat = "%3$s = mod %1$s %2$s";

  friend bool InterpretInstruction(ModInstruction<num_type> const &inst,
                                   interpreter::StackFrame &frame) {
    if constexpr (::interpreter::FitsInRegister<num_type>) {
      frame.set(inst.result, frame.resolve(inst.lhs) % frame.resolve(inst.rhs));
    } else {
      new (frame.resolve<addr_t>(inst.result)) num_type(
          *reinterpret_cast<num_type const *>(frame.resolve(inst.lhs)) %
          *reinterpret_cast<num_type const *>(frame.resolve(inst.rhs)));
    }
    return true;
  }

  void Apply(::interpreter::ExecutionContext &ctx) const
      requires(not ::interpreter::FitsInRegister<num_type>) {
    new (ctx.resolve<addr_t>(result))
        num_type(*reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(lhs)) %
                 *reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(rhs)));
  }

  num_type Resolve() const requires(::interpreter::FitsInRegister<num_type>) {
    return lhs.value() % rhs.value();
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
  using num_type = NumType;
  using operand_type =
      std::conditional_t<::interpreter::FitsInRegister<num_type>, num_type,
                         addr_t>;
  static constexpr std::string_view kDebugFormat = "%2$s = neg %1$s";

  friend bool InterpretInstruction(NegInstruction<num_type> const &inst,
                                   interpreter::StackFrame &frame) {
    if constexpr (::interpreter::FitsInRegister<num_type>) {
      frame.set(inst.result, -frame.resolve(inst.operand));
    } else {
      new (frame.resolve<addr_t>(inst.result)) num_type(
          -*reinterpret_cast<num_type const *>(frame.resolve(inst.operand)));
    }
    return true;
  }

  void Apply(::interpreter::ExecutionContext &ctx) const
      requires(not ::interpreter::FitsInRegister<num_type>) {
    new (ctx.resolve<addr_t>(result)) num_type(
        -*reinterpret_cast<num_type const *>(ctx.resolve<addr_t>(operand)));
  }

  num_type Resolve() const requires(::interpreter::FitsInRegister<num_type>) {
    return Apply(operand.value());
  }
  static num_type Apply(num_type operand) { return -operand; }

  RegOr<operand_type> operand;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_ARITHMETIC_H
