#include "ir/inliner.h"

#include "base/macros.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/instruction_inliner.h"
#include "ir/reg.h"
#include "ir/stack_frame_allocations.h"
#include "type/function.h"
#include "type/type.h"

namespace ir {

absl::flat_hash_map<std::string_view, BasicBlock *> Inline(
    Builder &bldr, Jump *to_be_inlined,
    absl::Span<ir::Results const> arguments,
    LocalBlockInterpretation const &block_interp) {
  DEBUG_LOG("inliner")(*to_be_inlined);
  if (to_be_inlined->work_item) {
    auto f = std::move(*to_be_inlined->work_item);
    if (f) { std::move(f)(); }
  }

  absl::flat_hash_map<std::string_view, BasicBlock *> result;

  auto *start_block          = bldr.CurrentBlock();
  size_t inlined_start_index = bldr.CurrentGroup()->blocks().size();

  InstructionInliner inl(to_be_inlined, bldr.CurrentGroup(), block_interp);

  size_t i = 0;
  for (type::Type const *t : to_be_inlined->type()->args()) {
    type::Apply(t, [&](auto tag) -> Reg {
      using T = typename decltype(tag)::type;
      return MakeReg(arguments[i++].get<T>(0));
    });
    // TODO Handle types not covered by Apply (structs, etc).
  }

  inl.InlineAllBlocks();

  // TODO Merge allocations

  bldr.CurrentBlock() = start_block;
  bldr.UncondJump(bldr.CurrentGroup()->blocks()[inlined_start_index]);

  return inl.ExtractNamedBlockMapping();
}

}  // namespace ir
