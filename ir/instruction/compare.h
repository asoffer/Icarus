#ifndef ICARUS_IR_INSTRUCTION_COMPARE_H
#define ICARUS_IR_INSTRUCTION_COMPARE_H

#include <string_view>

#include "base/extend.h"
#include "base/extend/serialize.h"
#include "base/extend/traverse.h"
#include "ir/instruction/debug.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace ir {

template <typename NumType>
struct EqInstruction : base::Extend<EqInstruction<NumType>>::template With<
                           base::BaseTraverseExtension,
                           base::BaseSerializeExtension, DebugFormatExtension> {
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%3$s = eq %1$s %2$s";

  bool Resolve() const { return Apply(lhs.value(), rhs.value()); }
  static bool Apply(num_type lhs, num_type rhs) { return lhs == rhs; }

  RegOr<num_type> lhs;
  RegOr<num_type> rhs;
  Reg result;
};

template <typename NumType>
struct NeInstruction : base::Extend<NeInstruction<NumType>>::template With<
                           base::BaseTraverseExtension,
                           base::BaseSerializeExtension, DebugFormatExtension> {
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%3$s = ne %1$s %2$s";

  bool Resolve() const { return Apply(lhs.value(), rhs.value()); }
  static bool Apply(num_type lhs, num_type rhs) { return lhs != rhs; }

  RegOr<num_type> lhs;
  RegOr<num_type> rhs;
  Reg result;
};

template <typename NumType>
struct LtInstruction : base::Extend<LtInstruction<NumType>>::template With<
                           base::BaseTraverseExtension,
                           base::BaseSerializeExtension, DebugFormatExtension> {
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%3$s = lt %1$s %2$s";

  bool Resolve() const { return Apply(lhs.value(), rhs.value()); }
  static bool Apply(num_type lhs, num_type rhs) { return lhs < rhs; }

  RegOr<num_type> lhs;
  RegOr<num_type> rhs;
  Reg result;
};

template <typename NumType>
struct LeInstruction : base::Extend<LeInstruction<NumType>>::template With<
                           base::BaseTraverseExtension,
                           base::BaseSerializeExtension, DebugFormatExtension> {
  using num_type                                 = NumType;
  static constexpr std::string_view kDebugFormat = "%3$s = le %1$s %2$s";

  bool Resolve() const { return Apply(lhs.value(), rhs.value()); }
  static bool Apply(num_type lhs, num_type rhs) { return lhs <= rhs; }

  RegOr<num_type> lhs;
  RegOr<num_type> rhs;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_COMPARE_H
