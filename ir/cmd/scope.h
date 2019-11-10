#ifndef ICARUS_IR_CMD_SCOPE_H
#define ICARUS_IR_CMD_SCOPE_H

#include "absl/types/span.h"
#include "compiler/compiler.h"
#include "ir/block_def.h"
#include "ir/cmd/util.h"

namespace ir {

struct BlockCmd {
  constexpr static cmd_index_t index = 39;

  static BasicBlock const *Execute(base::untyped_buffer::const_iterator *iter,
                                   std::vector<Addr> const &ret_slots,
                                   backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct ScopeCmd {
  constexpr static cmd_index_t index = 40;

  static BasicBlock const *Execute(base::untyped_buffer::const_iterator *iter,
                                   std::vector<Addr> const &ret_slots,
                                   backend::ExecContext *ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

// TODO "Handler" doesn't really make sense in the name for these.
Reg BlockHandler(compiler::Compiler *compiler,
                 absl::Span<RegOr<AnyFunc> const> befores,
                 absl::Span<RegOr<JumpHandler const *> const> afters);

Reg ScopeHandler(
    compiler::Compiler *compiler,
    absl::Span<RegOr<JumpHandler const *> const> inits,
    absl::Span<RegOr<AnyFunc> const> dones,
    absl::flat_hash_map<std::string_view, BlockDef *> const &blocks);

}  // namespace ir
#endif  // ICARUS_IR_CMD_SCOPE_H
