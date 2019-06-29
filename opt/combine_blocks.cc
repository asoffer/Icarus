#include "opt/combine_blocks.h"

#include "base/log.h"
#include "ir/compiled_fn.h"

#ifdef DBG
namespace debug {
bool optimize_ir = true;
}  // namespace debug
#endif  // DBG

namespace opt {

void RemoveEverythingAfterFirstJump(ir::BasicBlock* block) {
  auto& cmds = block->cmds_;
  auto iter  = cmds.begin();
  for (; iter != cmds.end(); ++iter) {
    switch (iter->op_code_) {
      case ir::Op::JumpPlaceholder: UNREACHABLE();
      case ir::Op::UncondJump:
      case ir::Op::CondJump:
      case ir::Op::ReturnJump: cmds.erase(++iter, cmds.end()); return;
      default: continue;
    }
  }
}

// Constructs a map from a block index to a pair. The first element of the pair
// is the number of incoming blocks. If that number is 1 and the unique incoming
// block is an unconditional jump, then the second element is that block.
// Otherwise, the value of the second pair is unspecified.
static absl::flat_hash_map<ir::BlockIndex, std::pair<size_t, ir::BlockIndex>>
IncomingMap(ir::CompiledFn* fn) {
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
  return incoming;
}

static void DeleteDeadBlocks(ir::CompiledFn* fn) {
  absl::flat_hash_set<ir::BlockIndex> alive;
  std::queue<ir::BlockIndex> processing;
  processing.push(ir::BlockIndex{0});
  while (!processing.empty()) {
    ir::BlockIndex current = processing.front();
    processing.pop();
    if (!alive.insert(current).second) { continue; }
    auto const& cmd = fn->block(current).cmds_.back();
    switch (cmd.op_code_) {
      case ir::Op::UncondJump: {
        processing.push(cmd.block_index_);
      } break;
      case ir::Op::CondJump: {
        processing.push(cmd.cond_jump_.blocks_[0]);
        processing.push(cmd.cond_jump_.blocks_[1]);
      } break;
      case ir::Op::ReturnJump: break;
      default: DEBUG_LOG()(*fn); UNREACHABLE();
    }
  }

  for (size_t i = 0; i < fn->blocks_.size(); ++i) {
    if (alive.contains(ir::BlockIndex(i))) { continue; }
    fn->blocks_.at(i).cmds_.clear();
  }
}

void CombineBlocks(ir::CompiledFn* fn) {
#ifdef DBG
  if (!debug::optimize_ir) return;
#endif  // DBG
  for (auto& block : fn->blocks_) { RemoveEverythingAfterFirstJump(&block); }
  DeleteDeadBlocks(fn);
  auto incoming = IncomingMap(fn);

  {
    absl::flat_hash_map<ir::BlockIndex, ir::BlockIndex> final_block;
    for (auto& [landing_block_index, count_and_jumping] : incoming) {
      auto [count, jumping_block_index] = count_and_jumping;
      if (count != 1 || jumping_block_index == ir::BlockIndex{}) { continue; }
      // landing_block_index -- the index of the block being jumped to
      // jumping_block_index -- the index of the block that jumps to
      //                        `landing_block_index`.
      //
      // If `jumping_block_index` is already present in the table, that means
      // we have already moved the jumping block (i.e., it was some other
      // block's landing block) and therefore is of no use. Instead we should
      // treat the block that jumped to it as our jumping block. But that
      // block may have been moved too! Thus, we repeatedly traverse until we
      // find a block a jumping block that has not yet been merged with the
      // one block that jumped to it.
      decltype(final_block)::const_iterator iter;
      while ((iter = final_block.find(jumping_block_index)) !=
             final_block.end()) {
        jumping_block_index = iter->second;
      }

      // Update the previous jump to cut down on the lookup path each time.
      // final_block[original_jumping_block_index] = jumping_block_index;
      final_block[landing_block_index] = jumping_block_index;

      final_block.emplace(landing_block_index, jumping_block_index);
      auto& jumping_block = fn->block(jumping_block_index);
      auto& landing_block = fn->block(landing_block_index);
      // Blocks may be empty if we've already found that they were
      // unreachable.
      if (jumping_block.cmds_.empty()) { continue; }
      jumping_block.Append(std::move(landing_block));
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

  for (auto& block : fn->blocks_) {
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
      default: DEBUG_LOG()(*fn); UNREACHABLE();
    }
  }
}

}  // namespace opt
