#ifndef ICARUS_COMPILER_BLOCK_GROUP_SCAFFOLDING_H
#define ICARUS_COMPILER_BLOCK_GROUP_SCAFFOLDING_H

#include <utility>

#include "absl/container/flat_hash_map.h"
#include "ast/scope.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/group.h"

namespace compiler {

// When constructing a block group, there are afew things that either must or
// are convinnent to do upfront rather than on the fly during IR emission.
// Specifically, stack allocations for local variables and destructors. This
// struct holds the mapping that IR emitters need to use the destruction paths.
struct BlockGroupScaffolding {
  ir::BasicBlock *EmitDestructionPath(ir::BasicBlock *from_block,
                                      ast::Scope const *from,
                                      ast::Scope const *to) {
    from_block->set_jump(
        ir::JumpCmd::Uncond(destruction_blocks.at(std::pair(from, to))));
    return landing_blocks.at(to);
  }
  // Each destruction path ending at the destruction of the local variables in a
  // scope continues executing at a particular basic block. This a map
  // associates each such scope to its corresponding landing block.
  absl::flat_hash_map<ast::Scope const *, ir::BasicBlock *> landing_blocks;

  // Given a destruction path starting at a scope `start` and proceeding to
  // destroy all local variables in all scopes up through and including a scope
  // `end`, this map associates the pair `{start, end}` with the block to which
  // you should jump to execute these destructions.
  absl::flat_hash_map<std::pair<ast::Scope const *, ast::Scope const *>,
                      ir::BasicBlock *>
      destruction_blocks;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_BLOCK_GROUP_SCAFFOLDING_H
