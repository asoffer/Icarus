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
    block->jump().Visit([&](auto const& j) {
      using type = std::decay_t<decltype(j)>;
      if constexpr (std::is_same_v<type, ir::JumpCmd::UncondJump>) {
        j.block->erase_incoming(block);
        if (j.block->incoming().empty()) { to_check.push(j.block); }
      } else if constexpr (std::is_same_v<type, ir::JumpCmd::CondJump>) {
        j.true_block->erase_incoming(block);
        if (j.true_block->incoming().empty()) { to_check.push(j.true_block); }

        j.false_block->erase_incoming(block);
        if (j.false_block->incoming().empty()) { to_check.push(j.false_block); }
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
             block->jump().Visit([](auto const& j) -> ir::BasicBlock* {
               using type = std::decay_t<decltype(j)>;
               if constexpr (std::is_same_v<type, ir::JumpCmd::UncondJump>) {
                 return j.block;
               } else {
                 return nullptr;
               }
             })) {
    DEBUG_LOG("opt")("Combining ", next_block, " into ", block);
    if (next_block->incoming().size() != 1) { break; }
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
    if (not block->instructions().empty()) { continue; }
    for (auto* inc : block->incoming()) {
      if (inc->jump().kind() == ir::JumpCmd::Kind::Uncond) {
        inc->Append(std::move(*block));
      } else if (inc->jump().kind() == ir::JumpCmd::Kind::Cond) {
        // TODO this might be okay, but it might be an issue with phi-nodes.
        // Even when it's not an issue for one branch it might be an issue if
        // more than one are combined.
        continue;
      } else if (block->jump().kind() == ir::JumpCmd::Kind::Uncond) {
        auto* target = ASSERT_NOT_NULL(block->jump().UncondTarget());
        inc->ReplaceJumpTargets(block.get(), target);
      }
    }

    if (block->incoming().empty()) {
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
    switch (block->incoming().size()) {
      case 0: dead_sources.push(block.get()); break;
      // There's no sense in doing block combining for dead blocks.
      // A block is the start of a chain of combinable blocks if either:
      // (a). It has one incoming block but it's parent has multiple outgoing
      // blocks. (b). It has multiple incoming blocks and a single outgoing
      // block. multiple incoming blocks
      case 1: {  // case (a).
        auto* inc_block = *block->incoming().begin();
        if (inc_block->jump().kind() == ir::JumpCmd::Kind::Cond) {
          CombineBlocksStartingAt(inc_block);
        }
      } break;
      default:  // case (b).
        if (block->jump().kind() == ir::JumpCmd::Kind::Uncond) {
          CombineBlocksStartingAt(block.get());
        }
        break;
    }
  }

  RemoveDeadBlocks(std::move(dead_sources), fn);
}

void RemoveTrivialFunctionCalls(ir::CompiledFn* fn) {
  for (auto& block : fn->mutable_blocks()) {
    for (auto const* inst : block->instructions()) {
      if (auto* call_inst = inst->if_as<ir::CallInstruction>()) {
        if (call_inst->func().is_reg()) { continue; }
        if (call_inst->func().value().kind() != ir::Fn::Kind::Native) {
          continue;
        }
        auto called_fn = call_inst->func().value().native();
        if (called_fn.type() != type::Func({}, {})) { continue; }
        ASSERT(called_fn->work_item == nullptr);
        // TODO track this as you go.
        for (auto const* called_block : called_fn->blocks()) {
          // What if it's null?
          if (not called_block->instructions().empty()) { goto next_inst; }
        }
        inst = nullptr;
      }
    next_inst:;
    }
  }
}

}  // namespace opt
