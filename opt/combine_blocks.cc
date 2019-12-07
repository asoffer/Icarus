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
        --j.block->num_incoming_;
        if (j.block->num_incoming_ == 0) { to_check.push(j.block); }
      } else if constexpr (std::is_same_v<type, ir::JumpCmd::CondJump>) {
        --j.true_block->num_incoming_;
        if (j.true_block->num_incoming_ == 0) { to_check.push(j.true_block); }

        --j.false_block->num_incoming_;
        if (j.false_block->num_incoming_ == 0) { to_check.push(j.false_block); }
      }
    });
    to_delete.insert(block);
    to_check.pop();
  }

  DeleteBlocks(to_delete, fn);
}

void CombineBlocks(ir::CompiledFn* fn) {
#if defined(ICARUS_DEBUG)
  if (not debug::optimize_ir) return;
#endif  // defined(ICARUS_DEBUG)

  std::queue<ir::BasicBlock*> dead_sources;

  // TODO use something like a base::bag
  for (auto& block : fn->mutable_blocks()) {
    switch (block->num_incoming_) {
      case 0: dead_sources.push(block.get()); break;
    }
  }

  RemoveDeadBlocks(std::move(dead_sources), fn);

}

}  // namespace opt
