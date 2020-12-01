#ifndef ICARUS_IR_INSTRUCTION_BYTE_VIEW_H
#define ICARUS_IR_INSTRUCTION_BYTE_VIEW_H

#include <string_view>

#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"

namespace ir {

struct ByteViewLengthInstruction
    : base::Extend<ByteViewLengthInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%2$s = byte-view length %1$s";

  uint64_t Resolve() const { return reg.value().get().size(); }

  RegOr<String> reg;
  Reg result;
};

struct ByteViewDataInstruction
    : base::Extend<ByteViewDataInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = byte-view data %1$s";

  ir::Addr Resolve() const { return reg.value().addr(); }

  RegOr<String> reg;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_BYTE_VIEW_H
