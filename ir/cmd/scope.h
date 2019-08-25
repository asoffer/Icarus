#ifndef ICARUS_IR_CMD_SCOPE_H
#define ICARUS_IR_CMD_SCOPE_H

#include "absl/types/span.h"
#include "ir/block.h"
#include "ir/cmd/util.h"

namespace ir {

struct BlockCmd {
  constexpr static cmd_index_t index = 39;

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator *iter,
                                           std::vector<Addr> const &ret_slots,
                                           backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);

  static void UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner);

};

Reg BlockHandler(absl::Span<RegOr<AnyFunc> const> befores,
                 absl::Span<RegOr<AnyFunc> const> afters);

}  // namespace ir
#endif  // ICARUS_IR_CMD_SCOPE_H
