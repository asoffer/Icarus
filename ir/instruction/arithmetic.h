#ifndef ICARUS_IR_INSTRUCTION_ARITHMETIC_H
#define ICARUS_IR_INSTRUCTION_ARITHMETIC_H

#include <string_view>

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/debug.h"

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
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%3$s = add %1$s %2$s";

  num_type Resolve() const { return lhs.value() + rhs.value(); }

  RegOr<num_type> lhs;
  RegOr<num_type> rhs;
  Reg result;
};

template <typename NumType>
struct SubInstruction
    : base::Extend<SubInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%3$s = sub %1$s %2$s";

  num_type Resolve() const { return lhs.value() - rhs.value(); }
  RegOr<num_type> lhs;
  RegOr<num_type> rhs;
  Reg result;
};

template <typename NumType>
struct MulInstruction
    : base::Extend<MulInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%3$s = mul %1$s %2$s";

  num_type Resolve() const { return lhs.value() * rhs.value(); }

  RegOr<num_type> lhs;
  RegOr<num_type> rhs;
  Reg result;
};

template <typename NumType>
struct DivInstruction
    : base::Extend<DivInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%3$s = div %1$s %2$s";

  num_type Resolve() const { return lhs.value() / rhs.value(); }

  RegOr<num_type> lhs;
  RegOr<num_type> rhs;
  Reg result;
};

template <typename NumType>
struct ModInstruction
    : base::Extend<ModInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%3$s = mod %1$s %2$s";

  num_type Resolve() const { return lhs.value() % rhs.value(); }

  RegOr<num_type> lhs;
  RegOr<num_type> rhs;
  Reg result;
};

template <typename NumType>
struct NegInstruction
    : base::Extend<NegInstruction<NumType>>::template With<
          base::BaseTraverseExtension, base::BaseSerializeExtension,
          DebugFormatExtension> {
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%2$s = neg %1$s";

  num_type Resolve() const { return Apply(operand.value()); }
  static num_type Apply(num_type operand) { return -operand; }

  RegOr<num_type> operand;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_ARITHMETIC_H
