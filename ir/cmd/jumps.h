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

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    std::string s;
    using base::stringify;
    switch (iter->read<Kind>()) {
      case Kind::kRet: s.append("ret"); break;
      case Kind::kUncond: s.append(stringify(iter->read<BlockIndex>())); break;
      case Kind::kCond: {
        s.append(stringify(iter->read<Reg>()));
        s.append(" false: ");
        s.append(stringify(iter->read<BlockIndex>()));
        s.append(", true: ");
        s.append(stringify(iter->read<BlockIndex>()));
      } break;
      default: UNREACHABLE();
    }
    return s;
  }

  static void UpdateForInlining(base::untyped_buffer::iterator* iter,
                                Inliner const& inliner) {
    auto& kind  = iter->read<Kind>();
    switch (kind) {
      case Kind::kRet:
        kind = Kind::kUncond;
        // We have ensured that any return jump has enough space to hold an
        // unconditional jump so that this write wile inlining does not need a
        // reallocation. This ensures iterators remain valid.
        iter->write(inliner.landing());
        break;
      case Kind::kUncond: inliner.Inline(&iter->read<BlockIndex>()); break;
      case Kind::kCond: {
        iter->read<Reg>();
        inliner.Inline(&iter->read<BlockIndex>());
        inliner.Inline(&iter->read<BlockIndex>());
      } break;
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
  // This extra block index is so that when inlined, we don't have to worry
  // about iterator invalidation, as a return becomes an unconditional jump
  // needing extra space.
  blk.cmd_buffer_.append(BlockIndex{});
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