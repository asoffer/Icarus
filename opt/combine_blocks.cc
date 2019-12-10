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
    next_block->jump_.Visit([&](auto const& j) {
      bldr.CurrentBlock() = block;
      using type = std::decay_t<decltype(j)>;
      if constexpr (std::is_same_v<type, ir::JumpCmd::UncondJump>) {
        bldr.UncondJump(j.block);
      } else if constexpr (std::is_same_v<type, ir::JumpCmd::CondJump>) {
        bldr.CondJump(j.reg, j.true_block, j.false_block);
      } else if constexpr (std::is_same_v<type, ir::JumpCmd::RetJump>) {
        // Nothing to do
      } else {
        UNREACHABLE();
      }
    });
    block->Append(std::move(*next_block));

    bldr.CurrentBlock() = next_block;
    bldr.ReturnJump();
  }
}

void ReduceEmptyBlocks(ir::CompiledFn* fn) {
  auto& bldr = ir::GetBuilder();
  for (auto& block : fn->mutable_blocks()) {
    if (not block->cmd_buffer_.empty()) { continue; }
    for (auto* inc : block->incoming_) {
      bldr.CurrentBlock() = inc;
      inc->jump_.Visit([&](auto const& inc_jump) {
        using inc_type = std::decay_t<decltype(inc_jump)>;
        if constexpr (std::is_same_v<inc_type, ir::JumpCmd::UncondJump>) {
          block->jump_.Visit([&](auto const& block_jump) {
            using block_type = std::decay_t<decltype(block_jump)>;
            if constexpr (std::is_same_v<block_type, ir::JumpCmd::UncondJump>) {
              bldr.UncondJump(block_jump.block);
            } else if constexpr (std::is_same_v<block_type,
                                                ir::JumpCmd::CondJump>) {
              bldr.CondJump(block_jump.reg, block_jump.true_block,
                            block_jump.false_block);
            } else if constexpr (std::is_same_v<block_type,
                                                ir::JumpCmd::RetJump>) {
              bldr.ReturnJump();
            } else {
              UNREACHABLE();
            }
          });
        } else if constexpr (std::is_same_v<inc_type, ir::JumpCmd::CondJump>) {
          if (inc_jump.true_block == block.get()) {
            block->jump_.Visit([&](auto const& block_jump) {
              using block_type = std::decay_t<decltype(block_jump)>;
              if constexpr (std::is_same_v<block_type,
                                           ir::JumpCmd::UncondJump>) {
                bldr.CondJump(inc_jump.reg, block_jump.block,
                              inc_jump.false_block);
              }
            });
          }

          if (inc_jump.false_block == block.get()) {
            block->jump_.Visit([&](auto const& block_jump) {
              using block_type = std::decay_t<decltype(block_jump)>;
              if constexpr (std::is_same_v<block_type,
                                           ir::JumpCmd::UncondJump>) {
                bldr.CondJump(inc_jump.reg, inc_jump.true_block,
                              block_jump.block);
              }
            });
          }
        } else if constexpr (std::is_same_v<inc_type, ir::JumpCmd::RetJump>) {
          // Nothing to do
        } else {
          UNREACHABLE();
        }
      });
    }
    // bldr.CurrentBlock() = block.get();
    // bldr.ReturnJump();
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
      case 1:  // case (a).
        if ((*block->incoming_.begin())->jump_.Visit([](auto const& j) {
              return std::is_same_v<std::decay_t<decltype(j)>,
                                    ir::JumpCmd::CondJump>;
            })) {
          CombineBlocksStartingAt(block.get());
        }
        break;
      default:  // case (b).
        if (block->jump_.Visit([](auto const& j) {
              return std::is_same_v<std::decay_t<decltype(j)>,
                                    ir::JumpCmd::UncondJump>;
            })) {
          CombineBlocksStartingAt(block.get());
        }
        break;
    }

  }

  RemoveDeadBlocks(std::move(dead_sources), fn);
}

}  // namespace opt
