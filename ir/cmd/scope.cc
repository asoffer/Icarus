#include "ir/cmd/scope.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "base/stringify.h"

namespace ir {

BasicBlock const *BlockCmd::Execute(base::untyped_buffer::const_iterator *iter,
                                    std::vector<Addr> const &ret_slots,
                                    backend::ExecContext *ctx) {
  auto *compiler                   = iter->read<compiler::Compiler *>();
  std::vector<AnyFunc> before_vals = internal::Deserialize<uint16_t, AnyFunc>(
      iter, [ctx](Reg reg) { return ctx->resolve<AnyFunc>(reg); });
  std::vector<AnyFunc> after_vals = internal::Deserialize<uint16_t, AnyFunc>(
      iter, [ctx](Reg reg) { return ctx->resolve<AnyFunc>(reg); });
  Reg result_reg = iter->read<Reg>();

  // TODO deal with leak.
  auto &frame = ctx->call_stack.top();
  frame.regs_.set(
      GetOffset(frame.fn_, result_reg),
      compiler->AddBlock(std::move(before_vals), std::move(after_vals)));
  return nullptr;
}

std::string BlockCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  iter->read<compiler::Compiler *>();
  std::vector<RegOr<AnyFunc>> before_vals =
      internal::Deserialize<uint16_t, AnyFunc>(
          iter, [](Reg reg) -> RegOr<AnyFunc> { return reg; });
  std::vector<RegOr<AnyFunc>> after_vals =
      internal::Deserialize<uint16_t, AnyFunc>(
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

Reg BlockHandler(compiler::Compiler *compiler,
                 absl::Span<RegOr<AnyFunc> const> befores,
                 absl::Span<RegOr<AnyFunc> const> afters) {
  auto &blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<BlockCmd>();
  blk.cmd_buffer_.append(compiler);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, befores);
  internal::Serialize<uint16_t>(&blk.cmd_buffer_, afters);
  Reg r = MakeResult<BlockDef const *>();
  blk.cmd_buffer_.append(r);
  return r;
}

BasicBlock const *ScopeCmd::Execute(base::untyped_buffer::const_iterator *iter,
                                    std::vector<Addr> const &ret_slots,
                                    backend::ExecContext *ctx) {
  auto *compiler = iter->read<compiler::Compiler *>();

  std::vector<AnyFunc> inits = internal::Deserialize<uint16_t, AnyFunc>(
      iter, [ctx](Reg reg) { return ctx->resolve<AnyFunc>(reg); });
  std::vector<AnyFunc> dones = internal::Deserialize<uint16_t, AnyFunc>(
      iter, [ctx](Reg reg) { return ctx->resolve<AnyFunc>(reg); });

  auto num_blocks = iter->read<uint16_t>();
  absl::flat_hash_map<std::string_view, BlockDef *> blocks;
  for (uint16_t i = 0; i < num_blocks; ++i) {
    auto name  = iter->read<std::string_view>();
    auto block = ctx->resolve<BlockDef *>(iter->read<BlockDef *>());
    blocks.emplace(name, block);
  }

  Reg result_reg = iter->read<Reg>();
  auto &frame    = ctx->call_stack.top();
  frame.regs_.set(GetOffset(frame.fn_, result_reg),
                  compiler->AddScope(std::move(inits), std::move(dones),
                                     std::move(blocks)));
  return nullptr;
}

std::string ScopeCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  // TODO for this to be okay, you do need to iterate through everything.
  return "scope()";
}

void ScopeCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                 Inliner const &inliner) {
  // TODO for this to be okay, you do need to iterate through everything.
}

Reg ScopeHandler(
    compiler::Compiler *compiler, absl::Span<RegOr<AnyFunc> const> inits,
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
