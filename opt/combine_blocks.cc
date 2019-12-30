#include "opt/combine_blocks.h"

#include "base/log.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"

#if defined(ICARUS_DEBUG)
namespace debug {
bool optimize_ir = true;
}  // namespace debug
#endif  // defined(ICARUS_DEBUG)

namespace opt {

static void DeleteBlocks(absl::flat_hash_set<ir::BasicBlock*> const& to_delete,
                         ir::CompiledFn* fn) {
  size_t head  = 1;
  size_t tail  = fn->blocks().size() - 1;
  auto& blocks = fn->mutable_blocks();
  while (head < tail) {
    if (to_delete.contains(blocks[head].get())) {
      blocks[head] = std::move(blocks[tail]);
      --tail;
    } else {
      ++head;
    }
  }
  if (head == tail and to_delete.contains(blocks[head].get())) {
    blocks[head] = nullptr;
    --tail;
  }

  DEBUG_LOG("opt")
  ("Num blocks reduced from ", fn->blocks().size(), " to ", tail + 1);
  fn->mutable_blocks().resize(tail + 1);
}

static void RemoveDeadBlocks(std::queue<ir::BasicBlock*> to_check,
                             ir::CompiledFn* fn) {
  absl::flat_hash_set<ir::BasicBlock*> to_delete;

  while (not to_check.empty()) {
    auto* block = to_check.front();
    block->jump_.Visit([&](auto const& j) {
      using type = std::decay_t<decltype(j)>;
      if constexpr (std::is_same_v<type, ir::JumpCmd::UncondJump>) {
        j.block->incoming_.erase(block);
        if (j.block->num_incoming() == 0) { to_check.push(j.block); }
      } else if constexpr (std::is_same_v<type, ir::JumpCmd::CondJump>) {
        j.true_block->incoming_.erase(block);
        if (j.true_block->num_incoming() == 0) { to_check.push(j.true_block); }

        j.false_block->incoming_.erase(block);
        if (j.false_block->num_incoming() == 0) {
          to_check.push(j.false_block);
        }
      }
    });
    to_delete.insert(block);
    to_check.pop();
  }

  DeleteBlocks(to_delete, fn);
}

void CombineBlocksStartingAt(ir::BasicBlock* block) {
  auto& bldr = ir::GetBuilder();

  while (auto* next_block =
             block->jump_.Visit([](auto const& j) -> ir::BasicBlock* {
               using type = std::decay_t<decltype(j)>;
               if constexpr (std::is_same_v<type, ir::JumpCmd::UncondJump>) {
                 return j.block;
               } else {
                 return nullptr;
               }
             })) {
    DEBUG_LOG("opt")("Combining ", next_block, " into ", block);
    if (next_block->num_incoming() != 1) { break; }
    block->Append(std::move(*next_block));

    // TODO should be part of moved-from state.
    bldr.CurrentBlock() = next_block;
    bldr.ReturnJump();
  }
}

void ReduceEmptyBlocks(ir::CompiledFn* fn) {
  auto& bldr       = ir::GetBuilder();
  auto& mut_blocks = fn->mutable_blocks();
  auto iter        = mut_blocks.begin();

  // Skip the initial block, it may be empty, but we've marked it as it's own
  // incoming so as to avoid considering it to be dead, and the code below would
  // incorrectly modify its jump.
  ++iter;

  for (; iter != mut_blocks.end(); ++iter) {
    auto& block = *iter;
    if (not block->instructions_.empty()) { continue; }
    for (auto* inc : block->incoming_) {
      if (inc->jump_.kind() == ir::JumpCmd::Kind::Uncond) {
        inc->Append(std::move(*block));
      } else if (block->jump_.kind() == ir::JumpCmd::Kind::Uncond) {
        auto* target = ASSERT_NOT_NULL(block->jump_.UncondTarget());
        inc->ReplaceJumpTargets(block.get(), target);
      }
    }

    if (block->num_incoming() == 0u) {
      bldr.CurrentBlock() = block.get();
      bldr.ReturnJump();
    }
  }
}

void CombineBlocks(ir::CompiledFn* fn) {
#if defined(ICARUS_DEBUG)
  if (not debug::optimize_ir) return;
#endif  // defined(ICARUS_DEBUG)

  std::queue<ir::BasicBlock*> dead_sources;

  // TODO use something like a base::bag
  for (auto& block : fn->mutable_blocks()) {
    switch (block->num_incoming()) {
      case 0: dead_sources.push(block.get()); break;
      // There's no sense in doing block combining for dead blocks.
      // A block is the start of a chain of combinable blocks if either:
      // (a). It has one incoming block but it's parent has multiple outgoing blocks.
      // (b). It has multiple incoming blocks and a single outgoing block.
      // multiple incoming blocks
      case 1: {  // case (a).
        auto* inc_block = *block->incoming_.begin();
        if (inc_block->jump_.kind() == ir::JumpCmd::Kind::Cond) {
          CombineBlocksStartingAt(inc_block);
        }
      } break;
      default:  // case (b).
        if (block->jump_.kind() == ir::JumpCmd::Kind::Uncond) {
          CombineBlocksStartingAt(block.get());
        }
        break;
    }

  }

  RemoveDeadBlocks(std::move(dead_sources), fn);
}

}  // namespace opt
