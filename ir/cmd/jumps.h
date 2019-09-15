#ifndef ICARUS_IR_CMD_JUMP_H
#define ICARUS_IR_CMD_JUMP_H

#include "base/debug.h"
#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "ir/reg_or.h"

namespace ir {

struct JumpCmd {
  constexpr static cmd_index_t index = 20;
  enum class Kind : uint8_t { kRet, kUncond, kCond };

  static BasicBlock const* Execute(base::untyped_buffer::const_iterator* iter,
                                   std::vector<Addr> const& ret_slots,
                                   backend::ExecContext* ctx) {
    switch (iter->read<Kind>()) {
      case Kind::kRet: return ReturnBlock();
      case Kind::kUncond: return iter->read<BasicBlock const*>();
      case Kind::kCond: {
        bool b           = ctx->resolve<bool>(iter->read<Reg>());
        auto false_block = iter->read<BasicBlock const*>();
        auto true_block  = iter->read<BasicBlock const*>();
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
      case Kind::kUncond: s.append(stringify(iter->read<BasicBlock const *>())); break;
      case Kind::kCond: {
        s.append(stringify(iter->read<Reg>()));
        s.append(" false: ");
        s.append(stringify(iter->read<BasicBlock const *>()));
        s.append(", true: ");
        s.append(stringify(iter->read<BasicBlock const *>()));
      } break;
      default: UNREACHABLE();
    }
    return s;
  }

  static void UpdateForInlining(base::untyped_buffer::iterator* iter,
                                Inliner const& inliner) {
    auto& kind = iter->read<Kind>();
    switch (kind) {
      case Kind::kRet:
        kind = Kind::kUncond;
        // We have ensured that any return jump has enough space to hold an
        // unconditional jump so that this write wile inlining does not need a
        // reallocation. This ensures iterators remain valid.
        iter->write(inliner.landing());
        break;
      case Kind::kUncond: inliner.Inline(iter->read<BasicBlock const *>()); break;
      case Kind::kCond: {
        iter->read<Reg>();
        inliner.Inline(iter->read<BasicBlock const *>());
        inliner.Inline(iter->read<BasicBlock const *>());
      } break;
      default: UNREACHABLE();
    }
  }
};

inline void UncondJump(BasicBlock const *block) {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<JumpCmd>();
  blk.cmd_buffer_.append(JumpCmd::Kind::kUncond);
  blk.cmd_buffer_.append(block);
}

inline void ReturnJump() {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<JumpCmd>();
  blk.cmd_buffer_.append(JumpCmd::Kind::kRet);
  // This extra block index is so that when inlined, we don't have to worry
  // about iterator invalidation, as a return becomes an unconditional jump
  // needing extra space.
  blk.cmd_buffer_.append(ReturnBlock());
}

inline void CondJump(RegOr<bool> cond, BasicBlock const* true_block,
                     BasicBlock const* false_block) {
  auto& blk = *GetBuilder().CurrentBlock();
  if (cond.is_reg()) {
    blk.cmd_buffer_.append_index<JumpCmd>();
    blk.cmd_buffer_.append(JumpCmd::Kind::kCond);
    blk.cmd_buffer_.append(cond.reg());
    blk.cmd_buffer_.append(false_block);
    blk.cmd_buffer_.append(true_block);
  } else {
    UncondJump(cond.value() ? true_block : false_block);
  }
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_JUMP_H
