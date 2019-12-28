#include "ir/inliner.h"

#include "base/macros.h"
#include "ir/builder.h"
#include "ir/cmd/basic.h"
#include "ir/cmd/call.h"
#include "ir/cmd/cast.h"
#include "ir/cmd/jump.h"
#include "ir/cmd/load.h"
#include "ir/cmd/misc.h"
#include "ir/cmd/phi.h"
#include "ir/cmd/print.h"
#include "ir/cmd/register.h"
#include "ir/cmd/return.h"
#include "ir/cmd/scope.h"
#include "ir/cmd/store.h"
#include "ir/cmd/types.h"
#include "ir/compiled_fn.h"
#include "ir/new_inliner.h"
#include "ir/reg.h"
#include "ir/stack_frame_allocations.h"
#include "type/function.h"
#include "type/type.h"

namespace ir {
namespace {

struct JumpInliner {
  // Constructs an Inliner which uses `bldr` to inline jumps.
  static JumpInliner Make(Builder &bldr) {
    return JumpInliner(bldr, bldr.CurrentGroup()->num_regs(),
                       bldr.CurrentGroup()->blocks().size() - 1);
  }

  BasicBlock *CopyBlock(BasicBlock const *block_to_copy) {
    auto *block = bldr_.AddBlock(*block_to_copy);
    block_updater_.emplace(block_to_copy, block);
    return block;
  }

  void Inline(base::unaligned_ref<Reg> r_ref,
              type::Type const *t = nullptr) const {
    Reg r = r_ref;
    if (r.is_arg()) {
      r_ref = Reg{r.arg_value() + reg_offset_};
    } else if (r.is_out()) {
      // NOT_YET();
    } else {
      r_ref = Reg{r.value() + reg_offset_};
    }

    if (t) {
      DEBUG_LOG("inline_reserve")("Reserving t = ", t->to_string());
      bldr_.CurrentGroup()->Reserve();
    }
  }

  BasicBlock *CorrespondingBlock(BasicBlock const *b) const {
    return block_updater_.at(b);
  }
  void Inline(BasicBlock **b) const { *b = CorrespondingBlock(*b); }

  void MergeAllocations(internal::BlockGroup *group,
                        StackFrameAllocations const &allocs) {}

  absl::flat_hash_map<BasicBlock const *, BasicBlock *> block_updater_;
 private:
  friend struct ::ir::internal::BlockGroup;
  explicit JumpInliner(Builder &bldr, size_t reg_offset, size_t block_offset)
      : bldr_(bldr), reg_offset_(reg_offset), block_offset_(block_offset) {}

  Builder &bldr_;
  size_t reg_offset_   = 0;
  size_t block_offset_ = 0;
};

}  // namespace

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

  // Note: It is important that the inliner is created before making registers
  // for each of the arguments, because creating the inliner looks at state on
  // the target function (counting which register it should start from), and
  // this should exclude the registers we create to hold the arguments.

  auto inliner = JumpInliner::Make(bldr);
  size_t reg_offset = bldr.CurrentGroup()->num_regs();

  auto *start_block          = bldr.CurrentBlock();
  size_t inlined_start_index = bldr.CurrentGroup()->blocks().size();

  for (auto *block_to_be_inlined : to_be_inlined->blocks()) {
    // Copy the block and then scan it for references to things that need to
    // be changed with inlining (e.g., basic blocks or registers).
    //
    // Note that we need to copy all blocks first (or at least allocate them)
    // because we may request a jump downwards (i.e., to a block which we have
    // not yet seen). In other words, we have to make sure that any jump which
    // needs to be updated, the block mapping is already present.
    inliner.CopyBlock(block_to_be_inlined);
  }

  std::vector<Reg> arg_regs;
  arg_regs.reserve(to_be_inlined->type()->args().size());
  size_t i = 0;
  for (type::Type const *t : to_be_inlined->type()->args()) {
    type::Apply(t, [&](auto tag) -> Reg {
      using T = typename decltype(tag)::type;
      return MakeReg(arguments[i++].get<T>(0));
    });
    // TODO Handle types not covered by Apply (structs, etc).
  }

  Inliner inl(reg_offset, inliner.block_updater_, nullptr);
  std::string_view chosen_block;
  for (auto *block_to_be_inlined : to_be_inlined->blocks()) {
    auto *block = inliner.CorrespondingBlock(block_to_be_inlined);
    DEBUG_LOG("inliner-before")(*block);

    for (auto &inst : block->instructions_) { inst->Inline(inl); }
    // block->jump_.Inline(inl);

    block->jump_.Visit([&](auto &j) {
      using type = std::decay_t<decltype(j)>;
      if constexpr (std::is_same_v<type, JumpCmd::RetJump>) {
        // TODO somehow we end up creating extra blocks that aren't used.
        // These blocks by default have a return jump at the end. We need to
        // clean these up but in the mean time, we can just ignore them.
      } else if constexpr (std::is_same_v<type, JumpCmd::UncondJump>) {
        inliner.Inline(&j.block);
        j.block->incoming_.insert(block);
      } else if constexpr (std::is_same_v<type, JumpCmd::CondJump>) {
        inliner.Inline(base::unaligned_ref{j.reg});
        inliner.Inline(&j.true_block);
        j.true_block->incoming_.insert(block);
        inliner.Inline(&j.false_block);
        j.false_block->incoming_.insert(block);
      } else if constexpr (std::is_same_v<type, JumpCmd::ChooseJump>) {
        std::string_view next_name;
        for (std::string_view name : j.blocks()) {
          if (name == "start" or name == "exit" or
              block_interp.block_node(name)) {
            next_name = name;
            break;
          }
        }

        // NOTE: `j` is no longer valid because we're overwriting it here.
        auto *entry_block   = bldr.AddBlock();
        bldr.CurrentBlock() = block;
        bldr.UncondJump(entry_block);
        result.emplace(next_name, entry_block);
      } else {
        static_assert(base::always_false<type>());
      }
    });

    DEBUG_LOG("inliner-after")(*block);
  }

  // TODO Merge allocations

  bldr.CurrentBlock() = start_block;
  bldr.UncondJump(bldr.CurrentGroup()->blocks()[inlined_start_index]);

  return result;
}

}  // namespace ir
