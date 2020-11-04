#ifndef ICARUS_IR_INSTRUCTION_INLINER_H
#define ICARUS_IR_INSTRUCTION_INLINER_H

#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/group.h"
#include "ir/instruction/jump.h"
#include "ir/local_block_interpretation.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"

namespace ir {
struct BasicBlock;

struct InstructionInliner {
  explicit InstructionInliner(internal::BlockGroupBase const *to_be_inlined,
                              internal::BlockGroupBase *into,
                              LocalBlockInterpretation block_interp);

  void Inline(Reg &r) const;
  void Inline(Value &v) const;
  void Inline(BasicBlock *&block, BasicBlock *incoming_block) const;
  void InlineJump(BasicBlock *block);

  template <typename T>
  void Inline(RegOr<T> &r) const {
    if (r.is_reg()) {
      Reg copy = r.reg();
      Inline(copy);
      r = copy;
    }
  }

  BasicBlock *InlineAllBlocks();

  absl::flat_hash_map<
      std::string_view,
      std::pair<BasicBlock *, core::Arguments<type::Typed<ir::Value>>>>
  ExtractNamedBlockMapping() && {
    return std::move(named_blocks_);
  }

  template <typename T>
  void Inline(std::vector<RegOr<T>> &rs) const {
    for (auto &r : rs) { Inline(r); }
  }

 private:
  BasicBlock *CorrespondingBlock(BasicBlock *block) {
    return blocks_.find(block)->second;
  }

  internal::BlockGroupBase const *to_be_inlined_;
  internal::BlockGroupBase *into_;
  int register_offset_;
  absl::flat_hash_map<BasicBlock const *, BasicBlock *> blocks_;

  absl::flat_hash_map<
      std::string_view,
      std::pair<BasicBlock *, core::Arguments<type::Typed<ir::Value>>>>
      named_blocks_;
  LocalBlockInterpretation block_interp_;
};

template <typename T>
struct InlineExtension {
  void Inline(InstructionInliner const &inliner) {
    auto inline_register = [&](auto &field) {
      using field_type = std::decay_t<decltype(field)>;
      if constexpr (base::meta<field_type> == base::meta<Reg> or
                    base::meta<field_type>.template is_a<ir::RegOr>()) {
        inliner.Inline(field);
      }
    };
    std::apply([&](auto &... field) { (inline_register(field), ...); },
               static_cast<T *>(this)->field_refs());
  }
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INLINER_H
