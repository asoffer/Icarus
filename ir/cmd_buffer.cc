#include "ir/cmd_buffer.h"

#include <string_view>
#include <type_traits>

#include "backend/exec.h"
#include "base/debug.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/load.h"
#include "ir/cmd/print.h"
#include "ir/cmd/register.h"
#include "ir/cmd/set_ret.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/compiled_fn.h"

namespace ir {

std::optional<BlockIndex> LegacyCmd::Execute(
    base::untyped_buffer::iterator* iter,
    std::vector<ir::Addr> const& ret_slots, backend::ExecContext* ctx) {
  auto block = ctx->ExecuteCmd(*iter->read<Cmd*>(), ret_slots);
  if (block == ir::BlockIndex{-2}) { return std::nullopt; }
  return block;
}

void LegacyCmd::UpdateForInlining(base::untyped_buffer::iterator* iter,
                                  Inliner const& inliner) {
  auto& cmd = *iter->read<Cmd*>();
  switch (cmd.op_code_) {
    case Op::Death: UNREACHABLE();
    case Op::Bytes:
    case Op::Align: {
      if (cmd.type_arg_.is_reg_) { inliner.Inline(&cmd.type_arg_.reg_); }
    } break;
    case Op::JumpPlaceholder: {
      NOT_YET();
    } break;

#define CASE(op_code, phi_type, args)                                          \
  case Op::op_code: {                                                          \
    NOT_YET();                                                                 \
    /*auto cmd_index = Phi(phi_type);                                          \
    auto reg       = CompiledFn::Current->Command(cmd_index).result;           \
    reg_relocs.emplace(cmd.result, reg);                                       \
    deferred_phis.emplace_back(cmd.args, cmd_index);                           \
    */                                                                         \
  } break
      CASE(PhiBool, type::Bool, phi_bool_);
      CASE(PhiInt8, type::Int8, phi_i8_);
      CASE(PhiInt16, type::Int16, phi_i16_);
      CASE(PhiInt32, type::Int32, phi_i32_);
      CASE(PhiInt64, type::Int64, phi_i64_);
      CASE(PhiNat8, type::Nat8, phi_u8_);
      CASE(PhiNat16, type::Nat16, phi_u16_);
      CASE(PhiNat32, type::Nat32, phi_u32_);
      CASE(PhiNat64, type::Nat64, phi_u64_);
      CASE(PhiFloat32, type::Float32, phi_float32_);
      CASE(PhiFloat64, type::Float64, phi_float64_);
      CASE(PhiType, type::Type_, phi_type_);
      // TODO CASE(PhiBlock, ____, phi_block_);
      // TODO CASE(PhiAddr, ____, phi_addr_);
      // TODO CASE(PhiEnum, ____, phi_enum_);
      // TODO CASE(PhiFlags, ____, phi_flags_);
      // TODO CASE(PhiFunc, ____, phi_func_);
#undef CASE
    case Op::GetRet: NOT_YET();
    case Op::ArgumentCache: NOT_YET();
    case Op::NewOpaqueType: NOT_YET();
    case Op::LoadSymbol: NOT_YET();
    case Op::Init: NOT_YET();
    case Op::Destroy: NOT_YET();
    case Op::Move: NOT_YET();
    case Op::Copy: NOT_YET();
    case Op::VerifyType: UNREACHABLE();
    case Op::EvaluateAsType: UNREACHABLE();
    case Op::CreateContext: UNREACHABLE();
    case Op::AddBoundConstant: UNREACHABLE();
    case Op::DestroyContext: UNREACHABLE();
    case Op::Call: {
      NOT_YET();
      // RegOr<AnyFunc> r_fn;
      // if (cmd.call_.fn_.is_reg_) {
      //   auto iter = reg_relocs.find(cmd.call_.fn_.reg_);
      //   if (iter == reg_relocs.end()) { goto next_block; }
      //   r_fn = iter->second.get<AnyFunc>(0).reg_;
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
    } break;
    case Op::PtrIncr: {
      if (cmd.ptr_incr_.ptr_.is_reg_) {
        inliner.Inline(&cmd.ptr_incr_.ptr_.reg_);
      }

      if (cmd.ptr_incr_.incr_.is_reg_) {
        inliner.Inline(&cmd.ptr_incr_.incr_.reg_);
      }
    } break;
    default:; NOT_YET(static_cast<int>(cmd.op_code_));
  }
}

BlockIndex CmdBuffer::Execute(std::vector<ir::Addr> const& ret_slots,
                              backend::ExecContext* ctx) {
  auto iter = buf_.begin();
  DEBUG_LOG("dbg")(buf_);
  while (true) {
    DEBUG_LOG("dbg")(buf_.begin(), buf_.size());
    ASSERT(iter < buf_.end());
    switch (iter.read<cmd_index_t>()) {
#define CASE(type)                                                             \
  case type::index: {                                                          \
    DEBUG_LOG("dbg")(#type);                                                   \
    auto result = type::Execute(&iter, ret_slots, ctx);                        \
    if (result.has_value()) { return *result; }                                \
  } break
      CASE(LegacyCmd);
      CASE(PrintCmd);
      CASE(AddCmd);
      CASE(SubCmd);
      CASE(MulCmd);
      CASE(DivCmd);
      CASE(ModCmd);
      CASE(NegCmd);
      CASE(NotCmd);
      CASE(LtCmd);
      CASE(LeCmd);
      CASE(EqCmd);
      CASE(NeCmd);
      CASE(GeCmd);
      CASE(GtCmd);
      CASE(StoreCmd);
      CASE(LoadCmd);
      CASE(VariantCmd);
      CASE(TupleCmd);
      CASE(ArrowCmd);
      CASE(PtrCmd);
      CASE(BufPtrCmd);
      CASE(JumpCmd);
      CASE(XorFlagsCmd);
      CASE(AndFlagsCmd);
      CASE(OrFlagsCmd);
      CASE(CastCmd);
      CASE(RegisterCmd);
      CASE(SetRetCmd);
#undef CASE
      default: UNREACHABLE();
    }
  }
}

void CmdBuffer::UpdateForInlining(Inliner const& inliner) {
  auto iter = buf_.begin();
  DEBUG_LOG("dbg")(buf_);

  while (iter < buf_.end()) {
    switch (iter.read<cmd_index_t>()) {
#define CASE(type)                                                             \
  case type::index:                                                            \
    DEBUG_LOG("dbg")(#type ": ", iter);                                        \
    type::UpdateForInlining(&iter, inliner);                                   \
    break
      CASE(LegacyCmd);
      CASE(PrintCmd);
      CASE(AddCmd);
      CASE(SubCmd);
      CASE(MulCmd);
      CASE(DivCmd);
      CASE(ModCmd);
      CASE(NegCmd);
      CASE(NotCmd);
      CASE(LtCmd);
      CASE(LeCmd);
      CASE(EqCmd);
      CASE(NeCmd);
      CASE(GeCmd);
      CASE(GtCmd);
      CASE(StoreCmd);
      CASE(LoadCmd);
      CASE(VariantCmd);
      CASE(TupleCmd);
      CASE(ArrowCmd);
      CASE(PtrCmd);
      CASE(BufPtrCmd);
      CASE(JumpCmd);
      CASE(XorFlagsCmd);
      CASE(AndFlagsCmd);
      CASE(OrFlagsCmd);
      CASE(CastCmd);
      CASE(RegisterCmd);
#undef CASE
    }
  }
  
  DEBUG_LOG("dbg")(buf_);
}

std::string CmdBuffer::to_string() const {
  // Come up with a better/more-permanent solution here.
  std::string s;
  auto iter = buf_.begin();
  while (iter < buf_.end()) {
    switch (iter.read<cmd_index_t>()) {
#define CASE(type)                                                             \
  case type::index:                                                            \
    s.append("\n" #type ": ");                                                 \
    s.append(type::DebugString(&iter));                                        \
    break
      CASE(LegacyCmd);
      CASE(PrintCmd);
      CASE(AddCmd);
      CASE(SubCmd);
      CASE(MulCmd);
      CASE(DivCmd);
      CASE(ModCmd);
      CASE(NegCmd);
      CASE(NotCmd);
      CASE(LtCmd);
      CASE(LeCmd);
      CASE(EqCmd);
      CASE(NeCmd);
      CASE(GeCmd);
      CASE(GtCmd);
      CASE(StoreCmd);
      CASE(LoadCmd);
      CASE(VariantCmd);
      CASE(TupleCmd);
      CASE(ArrowCmd);
      CASE(PtrCmd);
      CASE(BufPtrCmd);
      CASE(JumpCmd);
      CASE(XorFlagsCmd);
      CASE(AndFlagsCmd);
      CASE(OrFlagsCmd);
      CASE(CastCmd);
      CASE(RegisterCmd);
#undef CASE
    }
  }
  return s;
}

size_t GetOffset(CompiledFn const* fn, Reg r) {
  return fn->compiler_reg_to_offset_.at(r);
}
}  // namespace ir
