#include "ir/cmd/scope.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "base/stringify.h"

namespace ir {

std::string BlockCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  iter->read<compiler::Compiler *>();
  std::vector<RegOr<AnyFunc>> before_vals =
      internal::Deserialize<uint16_t, AnyFunc>(
          iter, [](Reg reg) -> RegOr<AnyFunc> { return reg; });
  std::vector<RegOr<JumpHandler const *>> after_vals =
      internal::Deserialize<uint16_t, JumpHandler const *>(
          iter, [](Reg reg) -> RegOr<JumpHandler const *> { return reg; });
  Reg result = iter->read<Reg>();

  using base::stringify;
  return absl::StrCat(
      stringify(result), " = before(",
      absl::StrJoin(before_vals, ", ",
                    [](std::string *out, RegOr<AnyFunc> f) {
                      return out->append(stringify(f));
                    }),
      ") after(",
      absl::StrJoin(after_vals, ", ",
                    [](std::string *out, RegOr<JumpHandler const *> f) {
                      return out->append(stringify(f));
                    }),

      ")");
}

Reg BlockHandler(compiler::Compiler *compiler,
                 absl::Span<RegOr<AnyFunc> const> befores,
                 absl::Span<RegOr<JumpHandler const *> const> afters) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<BlockCmd>();
  blk.cmd_buffer_.append(compiler);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, befores);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, afters);
  Reg r = MakeResult<BlockDef const *>();
  blk.cmd_buffer_.append(r);
  return r;
}

std::string ScopeCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  // TODO for this to be okay, you do need to iterate through everything.
  return "scope()";
}

Reg ScopeHandler(
    compiler::Compiler *compiler,
    absl::Span<RegOr<JumpHandler const *> const> inits,
    absl::Span<RegOr<AnyFunc> const> dones,
    absl::flat_hash_map<std::string_view, BlockDef *> const &blocks) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<ScopeCmd>();
  blk.cmd_buffer_.append(compiler);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, inits);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, dones);
  blk.cmd_buffer_.append<uint16_t>(blocks.size());
  for (auto [name, block] : blocks) {
    blk.cmd_buffer_.append(name);
    blk.cmd_buffer_.append(block);
  }
  Reg r = MakeResult<ScopeDef const *>();
  blk.cmd_buffer_.append(r);
  return r;
}

}  // namespace ir
