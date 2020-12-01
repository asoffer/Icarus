#ifndef ICARUS_IR_INSTRUCTION_ARITHMETIC_H
#define ICARUS_IR_INSTRUCTION_ARITHMETIC_H

#include <string_view>

#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"

namespace ir {

// TODO: Support all flavors of overflow behavior.
struct OverflowBehavior {
  enum Type { Undefined, Abort, Saturate, Wrap };
};

template <typename NumType,
          OverflowBehavior::Type Behavior = OverflowBehavior::Undefined>
struct AddInstruction
    : base::Extend<AddInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = add %1$s %2$s";

  NumType Resolve() const {
    if constexpr (Behavior == OverflowBehavior::Undefined) {
      return lhs.value() + rhs.value();
    } else if constexpr (Behavior == OverflowBehavior::Abort) {
      NOT_YET();
    } else if constexpr (Behavior == OverflowBehavior::Saturate) {
      NOT_YET();
    } else if constexpr (Behavior == OverflowBehavior::Wrap) {
      NOT_YET();
    }
  }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename NumType,
          OverflowBehavior::Type Behavior = OverflowBehavior::Undefined>
struct SubInstruction
    : base::Extend<SubInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = sub %1$s %2$s";

  NumType Resolve() const {
    if constexpr (Behavior == OverflowBehavior::Undefined) {
      return lhs.value() - rhs.value();
    } else if constexpr (Behavior == OverflowBehavior::Abort) {
      NOT_YET();
    } else if constexpr (Behavior == OverflowBehavior::Saturate) {
      NOT_YET();
    } else if constexpr (Behavior == OverflowBehavior::Wrap) {
      NOT_YET();
    }
  }
  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename NumType,
          OverflowBehavior::Type Behavior = OverflowBehavior::Undefined>
struct MulInstruction
    : base::Extend<MulInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = mul %1$s %2$s";

  NumType Resolve() const {
    if constexpr (Behavior == OverflowBehavior::Undefined) {
      return lhs.value() * rhs.value();
    } else if constexpr (Behavior == OverflowBehavior::Abort) {
      NOT_YET();
    } else if constexpr (Behavior == OverflowBehavior::Saturate) {
      NOT_YET();
    } else if constexpr (Behavior == OverflowBehavior::Wrap) {
      NOT_YET();
    }
  }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename NumType>
struct DivInstruction
    : base::Extend<DivInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = div %1$s %2$s";

  NumType Resolve() const { return lhs.value() / rhs.value(); }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename NumType>
struct ModInstruction
    : base::Extend<ModInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = mod %1$s %2$s";

  NumType Resolve() const { return lhs.value() % rhs.value(); }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename NumType>
struct NegInstruction
    : base::Extend<NegInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = neg %1$s";

  NumType Resolve() const { return Apply(operand.value()); }
  static NumType Apply(NumType operand) { return -operand; }

  RegOr<NumType> operand;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_ARITHMETIC_H
