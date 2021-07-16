#ifndef ICARUS_IR_INSTRUCTION_INLINER_H
#define ICARUS_IR_INSTRUCTION_INLINER_H

#include <string_view>
#include <utility>

#include "absl/container/flat_hash_map.h"
#include "base/debug.h"
#include "base/meta.h"
#include "ir/blocks/basic.h"
#include "ir/blocks/group.h"
#include "ir/instruction/jump.h"
#include "ir/local_block_interpretation.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace ir {
struct BasicBlock;

struct InstructionInliner {
  explicit InstructionInliner(internal::BlockGroupBase const *to_be_inlined,
                              internal::BlockGroupBase *into,
                              LocalBlockInterpretation block_interp);

  // By default there's nothing to do. We'll add overloads
  template <typename T>
  void Inline(T &v) const {
    constexpr auto type = base::meta<T>;
    if constexpr (type == base::meta<Reg>) {
      if (v.is_arg()) {
        v = Reg(v.arg_value() + register_offset_);
      } else {
        v = Reg(v.value() + register_offset_ + to_be_inlined_->num_args());
      }
    } else if constexpr (type.template is_a<RegOr>()) {
      if (v.is_reg()) {
        Reg copy = v.reg();
        Inline(copy);
        v = copy;
      }
    } else if constexpr (type.template is_a<std::vector>()) {
      for (auto &elem : v) { Inline(elem); }
    } else if constexpr (type.template is_a<std::pair>()) {
      Inline(v.first);
      Inline(v.second);
    }
  }

  void Inline(BasicBlock *&block, BasicBlock *incoming_block) const;
  void InlineJump(BasicBlock *block);

  BasicBlock *InlineAllBlocks();

  absl::flat_hash_map<std::string,
                      std::vector<std::pair<Arguments, BasicBlock *>>>
  ArgumentsByName() && {
    return std::move(arguments_by_name_);
  }

 private:
  BasicBlock *CorrespondingBlock(BasicBlock *block) {
    return blocks_.find(block)->second;
  }

  internal::BlockGroupBase const *to_be_inlined_;
  internal::BlockGroupBase *into_;
  int register_offset_;
  absl::flat_hash_map<BasicBlock const *, BasicBlock *> blocks_;
  absl::flat_hash_map<std::string,
                      std::vector<std::pair<Arguments, BasicBlock *>>>
      arguments_by_name_;
  absl::flat_hash_map<BasicBlock const *, Arguments> choose_argument_cache_;
  LocalBlockInterpretation block_interp_;
};

template <typename T>
struct InlineExtension {
  void Inline(InstructionInliner const &inliner) {
    std::apply([&](auto &... field) { (inliner.Inline(field), ...); },
               static_cast<T *>(this)->field_refs());
  }
};

}  // namespace ir

#endif  // ICARUS_IR_INSTRUCTION_INLINER_H
