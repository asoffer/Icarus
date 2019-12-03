#ifndef ICARUS_IR_CMD_INLINER_H
#define ICARUS_IR_CMD_INLINER_H

#include "ir/basic_block.h"
#include "ir/block_group.h"
#include "ir/builder.h"
#include "ir/jump.h"
#include "ir/local_block_interpretation.h"

namespace ir {

// Inlines a `Jump` into the corresponding target block group (function or
// jump). On exit, the current block of `bldr` will be the block the inlined
// jump lands on. The returned block name is the block we intend to jump to
// (which may need a function call for initialization first).
std::string_view Inline(Builder &bldr, Jump const *to_be_inlined,
                        absl::Span<ir::Results const> arguments,
                        LocalBlockInterpretation const &);

}  // namespace ir

#endif  // ICARUS_IR_CMD_INLINER_H
