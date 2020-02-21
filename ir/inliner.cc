#include "ir/inliner.h"

#include "base/macros.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/instruction_inliner.h"
#include "ir/value/reg.h"
#include "ir/stack_frame_allocations.h"
#include "type/function.h"
#include "type/type.h"

namespace ir {

absl::flat_hash_map<
    std::string_view,
    std::pair<BasicBlock *, core::FnArgs<type::Typed<ir::Results>>>>
Inline(Builder &bldr, Jump *to_be_inlined,
       absl::Span<ir::Results const> arguments,
       LocalBlockInterpretation const &block_interp) {
  DEBUG_LOG("inliner")(*to_be_inlined);
  if (to_be_inlined->work_item) {
    auto f = std::move(*to_be_inlined->work_item);
    if (f) { std::move(f)(); }
  }

  auto *start_block          = bldr.CurrentBlock();
  size_t inlined_start_index = bldr.CurrentGroup()->blocks().size();

  auto *into = bldr.CurrentGroup();
  InstructionInliner inl(to_be_inlined, into, block_interp);

  bldr.CurrentBlock() = start_block;
  size_t i            = 0;
  if (auto *state_type = to_be_inlined->type()->state()) {
    type::Apply(state_type, [&](auto tag) -> Reg {
      using T = typename decltype(tag)::type;
      return MakeReg(arguments[i++].get<T>(0));
    });
  }
  for (auto const& p : to_be_inlined->type()->params()) {
    type::Apply(p.value, [&](auto tag) -> Reg {
      using T = typename decltype(tag)::type;
      return MakeReg(arguments[i++].get<T>(0));
    });
    // TODO Handle types not covered by Apply (structs, etc).
  }

  inl.InlineAllBlocks();

  bldr.CurrentBlock() = start_block;
  bldr.UncondJump(bldr.CurrentGroup()->blocks()[inlined_start_index]);

  return inl.ExtractNamedBlockMapping();
}

}  // namespace ir
