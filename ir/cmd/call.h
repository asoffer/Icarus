#ifndef ICARUS_IR_CMD_CALL_H
#define ICARUS_IR_CMD_CALL_H

#include "absl/types/span.h"
#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "ir/reg_or.h"

namespace ir {
struct CallCmd {
  constexpr static cmd_index_t index = 38;

  static BasicBlock const *Execute(base::untyped_buffer::const_iterator *iter,
                                   std::vector<Addr> const &ret_slots,
                                   backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner);
};

void Call(RegOr<AnyFunc> const &fn, type::Function const *f,
          absl::Span<Results const> arguments);
void Call(RegOr<AnyFunc> const &fn, type::Function const *f,
          absl::Span<Results const> arguments, OutParams);

}  // namespace ir

#endif  // ICARUS_IR_CMD_CALL_H
