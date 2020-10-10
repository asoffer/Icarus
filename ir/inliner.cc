#include "ir/inliner.h"

#include "base/log.h"
#include "base/macros.h"
#include "ir/builder.h"
#include "ir/compiled_fn.h"
#include "ir/compiled_jump.h"
#include "ir/instruction/inliner.h"
#include "ir/value/reg.h"
#include "type/function.h"
#include "type/type.h"

namespace ir {

absl::flat_hash_map<
    std::string_view,
    std::pair<BasicBlock *, core::FnArgs<type::Typed<ir::Value>>>>
Inline(Builder &bldr, Jump to_be_inlined, absl::Span<ir::Value const> arguments,
       LocalBlockInterpretation const &block_interp) {
  LOG("inliner", "%s", to_be_inlined);
  if (ir::CompiledJump::From(to_be_inlined)->work_item) {
    auto f = std::move(*ir::CompiledJump::From(to_be_inlined)->work_item);
    if (f) { std::move(f)(); }
  }

  auto *start_block          = bldr.CurrentBlock();
  size_t inlined_start_index = bldr.CurrentGroup()->blocks().size();

  auto *into = bldr.CurrentGroup();
  InstructionInliner inl(ir::CompiledJump::From(to_be_inlined), into,
                         block_interp);

  bldr.CurrentBlock() = start_block;
  size_t i            = 0;
  auto const *jump_type = ir::CompiledJump::From(to_be_inlined)->type();
  if (auto *state_type = jump_type->state()) {
    type::Apply(state_type, [&](auto tag) -> Reg {
      using T = typename decltype(tag)::type;
      return MakeReg<RegOr<T>>(arguments[i++].get<RegOr<T>>());
    });
  }
  for (auto const &p : jump_type->params()) {
    type::Apply(p.value, [&](auto tag) -> Reg {
      using T = typename decltype(tag)::type;
      return MakeReg<RegOr<T>>(arguments[i++].get<RegOr<T>>());
    });
    // TODO Handle types not covered by Apply (structs, etc).
  }

  auto * entry = inl.InlineAllBlocks();

  bldr.CurrentBlock() = start_block;
  bldr.UncondJump(entry);

  return inl.ExtractNamedBlockMapping();
}

}  // namespace ir
