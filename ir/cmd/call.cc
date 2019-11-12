#include "ir/cmd/call.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ir/reg_or.h"

namespace ir {
std::string CallCmd::DebugString(base::untyped_buffer::const_iterator *iter) {
  bool fn_is_reg = iter->read<bool>();
  internal::ReadBits<uint16_t>(iter);

  using base::stringify;
  std::string result = fn_is_reg ? stringify(iter->read<Reg>())
                                 : stringify(iter->read<AnyFunc>());
  auto num_bytes_in_args = iter->read<core::Bytes>();
  iter->skip(num_bytes_in_args.value());
  uint16_t num_outs = iter->read<uint16_t>();
  std::vector<Reg> out_regs;
  out_regs.reserve(num_outs);
  for (uint16_t i = 0; i < num_outs; ++i) {
    out_regs.push_back(iter->read<Reg>());
  }
  absl::StrAppend(&result, " args[", stringify(num_bytes_in_args), "]: ",
                  absl::StrJoin(out_regs, ", ", [](std::string *out, Reg r) {
                    out->append(stringify(r));
                  }));
  return result;
}

namespace {

void CallImpl(BasicBlock *blk, RegOr<AnyFunc> const &fn,
              type::Function const *f, absl::Span<Results const> arguments) {
  ASSERT(arguments.size() == f->input.size());
  blk->cmd_buffer_.append_index<CallCmd>();
  blk->cmd_buffer_.append(fn.is_reg());
  internal::WriteBits<uint16_t, Results>(&blk->cmd_buffer_, arguments,
                                         [](Results const &r) {
                                           ASSERT(r.size() == 1u);
                                           return r.is_reg(0);
                                         });

  fn.apply([&](auto v) { blk->cmd_buffer_.append(v); });
  size_t bytes_written_slot = blk->cmd_buffer_.reserve<core::Bytes>();
  size_t arg_index          = 0;
  for (Results const &arg : arguments) {
    if (arg.is_reg(0)) {
      blk->cmd_buffer_.append(arg.get<Reg>(0));
    } else {
      type::Apply(f->input[arg_index], [&](auto tag) {
        using T = typename decltype(tag)::type;
        blk->cmd_buffer_.append(arg.get<T>(0).value());
      });
    }
    ++arg_index;
  }
  blk->cmd_buffer_.set(bytes_written_slot,
                       core::Bytes{blk->cmd_buffer_.size() -
                                   bytes_written_slot - sizeof(core::Bytes)});
}

}  // namespace

void Call(RegOr<AnyFunc> const &fn, type::Function const *f,
          absl::Span<Results const> arguments) {
  auto &blk = *GetBuilder().CurrentBlock();
  CallImpl(&blk, fn, f, arguments);
  blk.cmd_buffer_.append<uint16_t>(0);
}

void Call(RegOr<AnyFunc> const &fn, type::Function const *f,
          absl::Span<Results const> arguments, OutParams outs) {
  auto &blk = *GetBuilder().CurrentBlock();
  CallImpl(&blk, fn, f, arguments);

  blk.cmd_buffer_.append<uint16_t>(f->output.size());
  for (Reg r : outs.regs_) { blk.cmd_buffer_.append(r); }
}

}  // namespace ir
