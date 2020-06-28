#ifndef ICARUS_IR_INSTRUCTION_ARITHMETIC_H
#define ICARUS_IR_INSTRUCTION_ARITHMETIC_H

#include <string_view>

#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"
#include "ir/instruction/util.h"

namespace ir {

template <typename NumType>
struct AddInstruction
    : base::Extend<AddInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using binary                        = NumType;
  static constexpr cmd_index_t kIndex = internal::kAddInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();
  static constexpr std::string_view kDebugFormat = "%3$s = add %1$s %2$s";

  static NumType Apply(NumType lhs, NumType rhs) { return lhs + rhs; }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename NumType>
struct SubInstruction
    : base::Extend<SubInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using binary                        = NumType;
  static constexpr cmd_index_t kIndex = internal::kSubInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();
  static constexpr std::string_view kDebugFormat = "%3$s = sub %1$s %2$s";

  static NumType Apply(NumType lhs, NumType rhs) { return lhs - rhs; }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename NumType>
struct MulInstruction
    : base::Extend<MulInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using binary                        = NumType;
  static constexpr cmd_index_t kIndex = internal::kMulInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();
  static constexpr std::string_view kDebugFormat = "%3$s = mul %1$s %2$s";

  static NumType Apply(NumType lhs, NumType rhs) { return lhs * rhs; }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename NumType>
struct DivInstruction
    : base::Extend<DivInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using binary                        = NumType;
  static constexpr cmd_index_t kIndex = internal::kDivInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();
  static constexpr std::string_view kDebugFormat = "%3$s = div %1$s %2$s";

  static NumType Apply(NumType lhs, NumType rhs) { return lhs / rhs; }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

template <typename NumType>
struct ModInstruction
    : base::Extend<ModInstruction<NumType>>::template With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  using binary                        = NumType;
  static constexpr cmd_index_t kIndex = internal::kModInstructionRange.start +
                                        internal::PrimitiveIndex<NumType>();
  static constexpr std::string_view kDebugFormat = "%3$s = mod %1$s %2$s";

  static NumType Apply(NumType lhs, NumType rhs) { return lhs % rhs; }

  RegOr<NumType> lhs;
  RegOr<NumType> rhs;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_ARITHMETIC_H
