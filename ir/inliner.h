#ifndef ICARUS_IR_CMD_INLINER_H
#define ICARUS_IR_CMD_INLINER_H

#include "ir/basic_block.h"
#include "ir/block_group.h"
#include "ir/builder.h"
#include "ir/jump.h"
#include "ir/local_block_interpretation.h"

namespace ir {

// Inlines a `Jump` into the corresponding target block group (function or
// jump). Returns a mapping from the name of a block being jumped to, to the
// newly allocated BasicBlock that we use as the corresponding landing pad.
absl::flat_hash_map<std::string_view, BasicBlock *> Inline(
    Builder &bldr, Jump *to_be_inlined, absl::Span<ir::Results const> arguments,
    LocalBlockInterpretation const &);

}  // namespace ir

#endif  // ICARUS_IR_CMD_INLINER_H
