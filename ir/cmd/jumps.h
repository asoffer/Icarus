#ifndef ICARUS_IR_CMD_JUMP_H
#define ICARUS_IR_CMD_JUMP_H

#include <optional>

#include "base/debug.h"
#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"

namespace ir {

struct JumpCmd {
  constexpr static cmd_index_t index = 20;
  enum class Kind : uint8_t { kRet, kUncond, kCond };

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
                                           std::vector<Addr> const& ret_slots,
                                           backend::ExecContext* ctx) {
    switch (iter->read<Kind>()) {
      case Kind::kRet: return BlockIndex{-1};
      case Kind::kUncond: return iter->read<BlockIndex>();
      case Kind::kCond: {
        bool b           = ctx->resolve<bool>(iter->read<Reg>());
        auto false_block = iter->read<BlockIndex>();
        auto true_block  = iter->read<BlockIndex>();
        return b ? true_block : false_block;
      }
      default: UNREACHABLE();
    }
  }
};

inline void UncondJump(BlockIndex block) {
  auto& blk = GetBlock();
  blk.cmd_buffer_.append_index<JumpCmd>();
  blk.cmd_buffer_.append(JumpCmd::Kind::kUncond);
  blk.cmd_buffer_.append(block);
}

inline void ReturnJump() {
  auto& blk = GetBlock();
  blk.cmd_buffer_.append_index<JumpCmd>();
  blk.cmd_buffer_.append(JumpCmd::Kind::kRet);
}

inline void CondJump(RegOr<bool> cond, BlockIndex true_block,
                     BlockIndex false_block) {
  auto& blk = GetBlock();
  if (cond.is_reg_) {
    blk.cmd_buffer_.append_index<JumpCmd>();
    blk.cmd_buffer_.append(JumpCmd::Kind::kCond);
    blk.cmd_buffer_.append(cond.reg_);
    blk.cmd_buffer_.append(false_block);
    blk.cmd_buffer_.append(true_block);
  } else {
    UncondJump(cond.val_ ? true_block : false_block);
  }
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_JUMP_H
