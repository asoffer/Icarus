#include "ir/inliner.h"

#include "ir/builder.h"
#include "ir/cmd/jumps.h"
#include "ir/cmd/register.h"
#include "ir/compiled_fn.h"
#include "ir/reg.h"
#include "ir/stack_frame_allocations.h"
#include "type/function.h"
#include "type/type.h"

namespace ir {
void Inliner::Inline(Reg *r, type::Type const *t) const {
  if (r->is_arg()) {
    *r = Reg{r->arg_value() + reg_offset_};
  } else if (r->is_out()) {
    // NOT_YET();
  } else {
    *r = Reg{r->value() + reg_offset_};
  }

  if (t) {
    DEBUG_LOG("inline_reserve")("Reserving t = ", t->to_string());
    auto arch = core::Interpretter();
    auto offset =
        FwdAlign(GetBuilder().CurrentGroup()->reg_size_, t->alignment(arch));
    GetBuilder().CurrentGroup()->reg_size_ = offset + t->bytes(arch);
    GetBuilder().CurrentGroup()->reg_to_offset_.emplace(*r, offset.value());
  }
}

void Inliner::MergeAllocations(internal::BlockGroup *group,
                               StackFrameAllocations const &allocs) {}

std::pair<Results, bool> CallInline(
    CompiledFn *f, Results const &arguments,
    absl::flat_hash_map<BlockDef const *, BasicBlock *> const &block_map) {
  bool is_jump = false;  // TODO remove this
  std::vector<Results> return_vals;
  return_vals.resize(f->type_->output.size());

  // Note: It is important that the inliner is created before making registers
  // for each of the arguments, because creating the inliner looks state on the
  // current function (counting which register it should start on), and this
  // should exclude the registers we create to hold the arguments.
  auto inliner = GetBuilder().CurrentGroup()->inliner();

  std::vector<Reg> arg_regs;
  arg_regs.reserve(f->type_->input.size());
  for (size_t i = 0; i < f->type_->input.size(); ++i) {
    arg_regs.push_back(type::Apply(f->type_->input[i], [&](auto tag) -> Reg {
      using T = typename decltype(tag)::type;
      return MakeReg(arguments.get<T>(i));
    }));
  }

  size_t inlined_start = GetBuilder().CurrentGroup()->blocks().size();

  for (size_t i = 1; i < f->blocks().size(); ++i) {
    auto *block = GetBuilder().AddBlock();
    *block      = *std::move(f->blocks()[i]);
    block->cmd_buffer_.UpdateForInlining(inliner);
  }

  UncondJump(GetBuilder().CurrentGroup()->blocks()[inlined_start]);
  GetBuilder().CurrentBlock() = inliner.landing();

  inliner.MergeAllocations(GetBuilder().CurrentGroup(), f->allocs());

  Results results;
  for (auto const &r : return_vals) { results.append(r); }
  return std::pair{results, is_jump};
}

}  // namespace ir
