#include "ir/cmd/scope.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "base/stringify.h"

namespace ir {

std::optional<BlockIndex> BlockCmd::Execute(
    base::untyped_buffer::iterator *iter, std::vector<Addr> const &ret_slots,
    backend::ExecContext *ctx) {
  std::vector<AnyFunc> before_vals = internal::Deserialize<uint16_t, AnyFunc>(
      iter, [ctx](Reg &reg) { return ctx->resolve<AnyFunc>(reg); });
  std::vector<AnyFunc> after_vals = internal::Deserialize<uint16_t, AnyFunc>(
      iter, [ctx](Reg &reg) { return ctx->resolve<AnyFunc>(reg); });
  Reg result_reg = iter->read<Reg>();

  // TODO deal with leak.
  auto &frame = ctx->call_stack.top();
  frame.regs_.set(GetOffset(frame.fn_, result_reg),
                  new BlockDef(std::move(before_vals), std::move(after_vals)));
  return std::nullopt;
}

std::string BlockCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  std::vector<RegOr<AnyFunc>> before_vals = internal::Deserialize<uint16_t, AnyFunc>(
      iter, [](Reg reg) -> RegOr<AnyFunc> { return reg; });
  std::vector<RegOr<AnyFunc>> after_vals = internal::Deserialize<uint16_t, AnyFunc>(
      iter, [](Reg reg) -> RegOr<AnyFunc> { return reg; });
  Reg result = iter->read<Reg>();

  using base::stringify;
  return absl::StrCat(stringify(result), " = before(",
                      absl::StrJoin(before_vals, ", ",
                                    [](std::string *out, RegOr<AnyFunc> f) {
                                      return out->append(stringify(f));
                                    }),
                      ") after(",
                      absl::StrJoin(after_vals, ", ",
                                    [](std::string *out, RegOr<AnyFunc> f) {
                                      return out->append(stringify(f));
                                    }),

                      ")");
}

void BlockCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                 Inliner const &inliner) {}

Reg BlockHandler(absl::Span<RegOr<AnyFunc> const> befores,
                 absl::Span<RegOr<AnyFunc> const> afters) {
  auto &blk = GetBuilder().function()->block(GetBuilder().CurrentBlock());
  blk.cmd_buffer_.append_index<BlockCmd>();
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, befores);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, afters);
  Reg r = MakeResult<BlockDef const *>();
  blk.cmd_buffer_.append(r);
  return r;
}

}  // namespace ir
