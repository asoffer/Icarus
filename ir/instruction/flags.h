#ifndef ICARUS_IR_INSTRUCTION_FLAGS_H
#define ICARUS_IR_INSTRUCTION_FLAGS_H

#include <string_view>

#include "base/extend.h"
#include "ir/byte_code_writer.h"
#include "ir/instruction/debug.h"
#include "ir/instruction/inliner.h"
#include "ir/instruction/op_codes.h"
#include "ir/instruction/util.h"

namespace ir {

struct XorFlagsInstruction
    : base::Extend<XorFlagsInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = xor-flags %1$s %2$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(result,
                                  Apply(ctx.resolve(lhs), ctx.resolve(rhs)));
  }
  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs ^ rhs; }

  RegOr<FlagsVal> lhs;
  RegOr<FlagsVal> rhs;
  Reg result;
};

struct AndFlagsInstruction
    : base::Extend<AndFlagsInstruction>::With<
          ByteCodeExtension, InlineExtension, DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = and-flags %1$s %2$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(result,
                                  Apply(ctx.resolve(lhs), ctx.resolve(rhs)));
  }
  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs & rhs; }

  RegOr<FlagsVal> lhs;
  RegOr<FlagsVal> rhs;
  Reg result;
};

struct OrFlagsInstruction
    : base::Extend<OrFlagsInstruction>::With<ByteCodeExtension, InlineExtension,
                                             DebugFormatExtension> {
  static constexpr std::string_view kDebugFormat = "%3$s = or-flags %1$s %2$s";

  void Apply(interpretter::ExecutionContext& ctx) const {
    ctx.current_frame().regs_.set(result,
                                  Apply(ctx.resolve(lhs), ctx.resolve(rhs)));
  }
  static FlagsVal Apply(FlagsVal lhs, FlagsVal rhs) { return lhs | rhs; }

  RegOr<FlagsVal> lhs;
  RegOr<FlagsVal> rhs;
  Reg result;
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_FLAGS_H
