#include "ir/cmd/call.h"

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "backend/exec.h"
#include "ir/reg_or.h"

namespace ir {
namespace {

type::Function const *GetType(AnyFunc f) {
  return f.is_fn() ? f.func()->type_
                   : &f.foreign().type()->as<type::Function>();
}

}  // namespace

BasicBlock const *CallCmd::Execute(base::untyped_buffer::const_iterator *iter,
                                   std::vector<Addr> const &ret_slots,
                                   backend::ExecContext *ctx) {
  bool fn_is_reg                = iter->read<bool>();
  std::vector<bool> is_reg_bits = internal::ReadBits<uint16_t>(iter);

  AnyFunc f = fn_is_reg ? ctx->resolve<AnyFunc>(iter->read<Reg>())
                        : iter->read<AnyFunc>();
  type::Function const *fn_type = GetType(f);
  DEBUG_LOG("call")(f, ": ", fn_type->to_string());
  DEBUG_LOG("call")(is_reg_bits);

  iter->read<core::Bytes>();

  base::untyped_buffer call_buf;
  ASSERT(fn_type->input.size() == is_reg_bits.size());
  for (size_t i = 0; i < is_reg_bits.size(); ++i) {
    type::Type const *t = fn_type->input[i];
    if (is_reg_bits[i]) {
      auto reg = iter->read<Reg>();
      PrimitiveDispatch(PrimitiveIndex(t), [&](auto tag) {
        using type = typename std::decay_t<decltype(tag)>::type;
        call_buf.append(ctx->resolve<type>(reg));
      });

    } else if (t->is_big()) {
      NOT_YET();
    } else {
      PrimitiveDispatch(PrimitiveIndex(t), [&](auto tag) {
        using type = typename std::decay_t<decltype(tag)>::type;
        call_buf.append(iter->read<type>());
      });
    }
  }

  uint16_t num_rets = iter->read<uint16_t>();
  std::vector<Addr> return_slots;
  return_slots.reserve(num_rets);
  for (uint16_t i = 0; i < num_rets; ++i) {
    auto reg = iter->read<Reg>();
    // TODO: handle is_loc outparams.
    // NOTE: This is a hack using heap address slots to represent registers
    // since they are both void* and are used identically in the interpretter.
    auto addr = ir::Addr::Heap(ctx->call_stack.top().regs_.raw(
        ctx->call_stack.top().fn_->compiler_reg_to_offset_.at(reg)));
    DEBUG_LOG("call")("Ret addr = ", addr);
    return_slots.push_back(addr);
  }

  backend::Execute(f, call_buf, return_slots, ctx);
  return nullptr;
}

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

void CallCmd::UpdateForInlining(base::untyped_buffer::iterator *iter,
                                Inliner const &inliner) {
  // RegOr<AnyFunc> r_fn;
  // if (cmd.call_.fn_.is_reg()) {
  //   auto iter = reg_relocs.find(cmd.call_.fn_.reg());
  //   if (iter == reg_relocs.end()) { goto next_block; }
  //   r_fn = iter->second.get<AnyFunc>(0).reg();
  // } else {
  //   r_fn = cmd.call_.fn_;
  // }

  // Results new_arg_results;
  // for (size_t i = 0; i < cmd.call_.arguments_->results().size(); ++i) {
  //   if (cmd.call_.arguments_->results().is_reg(i)) {
  //     auto iter =
  //         reg_relocs.find(cmd.call_.arguments_->results().get<Reg>(i));
  //     if (iter == reg_relocs.end()) { goto next_block; }
  //     new_arg_results.append(iter->second.GetResult(0));
  //   } else {
  //     new_arg_results.append(
  //         cmd.call_.arguments_->results().GetResult(i));
  //   }
  // }
  // Arguments new_args(cmd.call_.arguments_->type_,
  //                    std::move(new_arg_results));

  // if (cmd.call_.outs_) {
  //   OutParams outs;
  //   for (size_t i = 0; i < cmd.call_.outs_->regs_.size(); ++i) {
  //     if (cmd.call_.outs_->is_loc_[i]) {
  //       auto old_r = cmd.call_.outs_->regs_[i];
  //       auto iter  = reg_relocs.find(old_r);
  //       if (iter == reg_relocs.end()) { goto next_block; }
  //       // TODO reg_relocs.emplace(, op_fn(r0, r1));
  //     } else {
  //       auto r =
  //           Reserve(type::Int64);  // TODO this type is probably wrong.
  //       outs.is_loc_.push_back(false);
  //       outs.regs_.push_back(r);
  //       reg_relocs.emplace(cmd.call_.outs_->regs_[i], r);
  //     }
  //   }
  //   Call(r_fn, std::move(new_args), std::move(outs));
  // } else {
  //   Call(r_fn, std::move(new_args));
  // }
  NOT_YET();
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
