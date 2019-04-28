#include "opt/combine_blocks.h"

#include "base/log.h"
#include "ir/compiled_fn.h"

namespace opt {

void CombineBlocks(ir::CompiledFn* fn) {
  for (auto& block : fn->blocks_) {
    auto& cmds = block.cmds_;
    auto iter  = cmds.begin();
    for (; iter != cmds.end(); ++iter) {
      switch (iter->op_code_) {
        case ir::Op::JumpPlaceholder:
          return;  // Don't bother optimizing this kind of block.
        case ir::Op::UncondJump:
        case ir::Op::CondJump:
        case ir::Op::ReturnJump:
        case ir::Op::BlockSeqJump:
          cmds.erase(++iter, cmds.end());
          goto next_block;
        default: continue;
      }
    }
  next_block:;
  }

  // Maps a block index to a pair. The first element of the pair is the number
  // of incoming blocks. If that number is 1, and the unique incoming block is
  // an unconditional jump, then the second element is that block. Otherwise,
  // the value of the second pair is unspecified.
  absl::flat_hash_map<ir::BlockIndex, std::pair<size_t, ir::BlockIndex>>
      incoming;

  for (int32_t i = 0; i < static_cast<int32_t>(fn->blocks_.size()); ++i) {
    auto& cmds = fn->blocks_.at(i).cmds_;
    if (cmds.empty()) { continue; }
    switch (cmds.back().op_code_) {
      case ir::Op::UncondJump: {
        auto& [num, incoming_block] = incoming[cmds.back().block_index_];
        ++num;
        incoming_block = ir::BlockIndex{i};
      } break;
      case ir::Op::CondJump:
        ++incoming[cmds.back().cond_jump_.blocks_[0]].first;
        ++incoming[cmds.back().cond_jump_.blocks_[1]].first;
        break;
      default:;
    }
  }

  {
    absl::flat_hash_map<ir::BlockIndex, ir::BlockIndex> final_block;
    for (auto& [landing_block_index, count_and_jumping] : incoming) {
      auto [count, jumping_block_index] = count_and_jumping;
      if (count != 1) { continue; }
      if (jumping_block_index == ir::BlockIndex{}) { continue; }
      auto iter = final_block.find(jumping_block_index);
      if (iter != final_block.end()) { jumping_block_index = iter->second; }
      final_block.emplace(landing_block_index, jumping_block_index);
      auto& jumping_block = fn->block(jumping_block_index);
      auto& landing_block = fn->block(landing_block_index);
      ASSERT(jumping_block.cmds_.back().op_code_ == ir::Op::UncondJump);
      jumping_block.cmds_.pop_back();
      jumping_block.cmds_.insert(
          jumping_block.cmds_.end(),
          std::make_move_iterator(landing_block.cmds_.begin()),
          std::make_move_iterator(landing_block.cmds_.end()));
      landing_block.cmds_.clear();
    }
  }

  ir::BlockIndex head{0};
  ir::BlockIndex tail{static_cast<int32_t>(fn->blocks_.size()) - 1};
  absl::flat_hash_map<ir::BlockIndex, ir::BlockIndex> remap;

  while (head.value < tail.value) {
    if (auto& tail_block = fn->block(tail); tail_block.cmds_.empty()) {
      --tail.value;
    } else if (auto& head_block = fn->block(head); !head_block.cmds_.empty()) {
      ++head.value;
    } else {
      head_block = std::move(tail_block);
      remap.emplace(tail, head);
    }
  }
  ASSERT(head.value == tail.value);
  if (auto& block = fn->block(head); block.cmds_.empty()) { --head.value; }
  fn->blocks_.resize(head.value + 1);

  for (auto &block : fn->blocks_) {
    if (block.cmds_.empty()) { continue; }
    ASSERT(block.cmds_.empty() == false);
    auto& cmd = block.cmds_.back();
    switch (cmd.op_code_) {
      case ir::Op::UncondJump: {
        if (auto iter = remap.find(cmd.block_index_); iter != remap.end()) {
          cmd.block_index_ = iter->second;
        }
      } break;
      case ir::Op::CondJump: {
        if (auto iter = remap.find(cmd.cond_jump_.blocks_[0]);
            iter != remap.end()) {
          cmd.cond_jump_.blocks_[0] = iter->second;
        }
        if (auto iter = remap.find(cmd.cond_jump_.blocks_[1]);
            iter != remap.end()) {
          cmd.cond_jump_.blocks_[1] = iter->second;
        }
      } break;
      case ir::Op::ReturnJump: break;
      default: UNREACHABLE();
    }
  }
}

}  // namespace opt
