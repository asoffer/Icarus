#ifndef ICARUS_IR_INSTRUCTION_BYTE_VIEW_H
#define ICARUS_IR_INSTRUCTION_BYTE_VIEW_H

#include <string_view>

#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"
#include "ir/interpretter/execute.h"

namespace ir {

struct ByteViewLengthInstruction
    : base::Extend<ByteViewLengthInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat =
      "%2$s = byte-view length %1$s";

  void Apply(interpretter::ExecutionContext& ctx) {
    ctx.current_frame().regs_.set(result,
                                  ctx.resolve<ir::String>(reg).get().size());
  }

  Reg reg;
  Reg result;
};

struct ByteViewDataInstruction
    : base::Extend<ByteViewDataInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%2$s = byte-view data %1$s";

  void Apply(interpretter::ExecutionContext& ctx) {
    ctx.current_frame().regs_.set(result, ctx.resolve<ir::String>(reg).addr());
  }

  Reg reg;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_BYTE_VIEW_H
