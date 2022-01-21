#ifndef ICARUS_COMPILER_SUBROUTINE_SCAFFOLDING_H
#define ICARUS_COMPILER_SUBROUTINE_SCAFFOLDING_H

#include <utility>

#include "absl/container/flat_hash_map.h"
#include "ast/scope.h"
#include "ir/basic_block.h"
#include "ir/subroutine.h"

namespace compiler {

// When constructing a subroutine, there are afew things that either must or are
// convinnent to do upfront rather than on the fly during IR emission.
// Specifically, stack allocations for local variables and destructors. This
// struct holds the mapping that IR emitters need to use the destruction paths.
struct SubroutineScaffolding {
  // Allocations for local variables
  absl::flat_hash_map<ast::Declaration::Id const *, ir::RegOr<ir::addr_t>>
      stack_allocations;

  ir::BasicBlock *EmitDestructionPath(ir::BasicBlock *from_block,
                                      ast::Scope const *from,
                                      ast::Scope const *to) {
    auto iter               = destruction_blocks.find(std::pair(from, to));
    ir::BasicBlock *landing = landing_blocks.at(to);
    from_block->set_jump(ir::JumpCmd::Uncond(
        iter != destruction_blocks.end() ? iter->second : landing));
    return landing;
  }
  // Each destruction path ending at the destruction of the local variables in a
  // scope continues executing at a particular basic block. This a map
  // associates each such scope to its corresponding landing block.
  absl::flat_hash_map<ast::Scope const *, ir::BasicBlock *> landing_blocks;

  // Variable destruction can be quite complex. Within a single scope, variables
  // are destroyed in reverse order of declaration. However, nested scopes can
  // have early return statements which complicate things. As an example:
  //
  // ```
  // func ::= (condition: bool) -> () {
  //   a: A
  //   b: B
  //   if (condition) {
  //     c: C
  //     return
  //   }
  //   d: D
  // }
  // ```
  //
  // In this example, there are two possible destruction sequences. The sequence
  // for `f`'s body is simply destroying the variables in reverse order of
  // occurence: `d`, then `b`, then `a`. However, if `condition` is true, we
  // need to execute a different sequence: `c`, then `b`, then `a`. One might
  // therefore hope that we could build a tree where each node represents a
  // declaration, and has a parent which is the declaration to be destroyed
  // immediately following it. In this case, such a tree would have `c` and `d`
  // each be children of `b` which would be a child of the root `a`.
  //
  // This is almost correct, but there are several cases where this doesn't
  // quite hold. First, while-loops may allocate their local variables and
  // destroy them many times. We would not want them to be part of such a tree
  // because destruction should not unconditionally proceed all the way to the
  // root. Similarly, if-statements may fall-through to the parent scope and
  // continue execution. Whatsmore, scopes can explicitly yield to ancestor
  // scopes (not just their parent). In such cases we would want to destroy
  // every declared variable between the current location and the location being
  // jumped to. None of these fit directly with the single-tree data structure
  // described above.
  //
  // It's worth noting that each of the cases mentioned above are actually all
  // the same case: They all involve destroying local variables in reverse order
  // of declaration until a particular point is reachedd. When we described the
  // tree structure, we implicitly assumed that ending point was the function
  // scope itself, however, many such points can exist. Thus, a valid solution
  // to this problem is to have a destruction path tree for each location that
  // may be jumped to.
  //
  // To illustrate, consider the example:
  //
  // ```
  // func ::= (condition1: bool, condition2: bool) -> () {
  //   a: A
  //   b: B
  //   #.label if (condition1) {
  //     c: C
  //     return
  //   } else {
  //     d: D
  //     if (condition2) {
  //       e: E
  //       #.label <<
  //     } else {
  //       return
  //     }
  //   }
  //   f: F
  // }
  // ```
  //
  // Here there are two possible locations destruction paths may stop: The
  // function return, and the label `#.label`.
  //
  // The destruction tree for the return looks like:
  //
  //           --> c
  //          /
  //   a -> b ---> d
  //          \
  //           --> f
  //
  // The destruction tree for `#.label` looks like:
  //
  //   d -> e
  //
  // The map `destruction_blocks` declared below holds the information
  // describing to which block one should jump if they want to execute a
  // particular destruction path. The key is a pair of nodes representing
  // relevant code points. The first value is the landing location and the
  // second is the location you are starting from. The mapped value is the block
  // to which you should jump to execute that sequence of destructors.
  absl::flat_hash_map<std::pair<ast::Scope const *, ast::Scope const *>,
                      ir::BasicBlock *>
      destruction_blocks;
};

}  // namespace compiler

#endif  // ICARUS_COMPILER_SUBROUTINE_SCAFFOLDING_H
