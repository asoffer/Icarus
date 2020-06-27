#ifndef ICARUS_IR_INSTRUCTION_BYTE_VIEW_H
#define ICARUS_IR_INSTRUCTION_BYTE_VIEW_H

#include <string_view>

#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"

namespace ir {

struct ByteViewLengthInstruction
    : base::Extend<ByteViewLengthInstruction>::With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr cmd_index_t kIndex =
      internal::kByteViewLengthInstructionNumber;
  static constexpr std::string_view kDebugFormat =
      "%2$s = byte-view length %1$s";

  Reg reg;
  Reg result;
};

struct ByteViewDataInstruction
    : base::Extend<ByteViewDataInstruction>::With<
          WriteByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr cmd_index_t kIndex =
      internal::kByteViewDataInstructionNumber;
  static constexpr std::string_view kDebugFormat = "%2$s = byte-view data %1$s";

  Reg reg;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_BYTE_VIEW_H
