#include "property/property_map.h"

#include "ir/func.h"

namespace prop {
BlockStateView::BlockStateView(const IR::BasicBlock *block)
    : view_(block->cmds_.size()) {}

FnStateView::FnStateView(IR::Func *fn) {
  for (const auto &block : fn->blocks_) {
    view_.emplace(&block, BlockStateView{&block});
  }
}

PropertyMap::PropertyMap(IR::Func *fn) : fn_(fn) {
  // TODO copy fnstateview rather than creating it repeatedly?
  for (const auto &block : fn_->blocks_) {
    view_.emplace(&block, FnStateView(fn_));
  }

  for (const auto &block1 : fn_->blocks_) {
    for (i32 block_index = 0;
         block_index < static_cast<i32>(fn_->blocks_.size()); ++block_index) {
      i32 len = fn_->block(IR::BlockIndex{block_index}).cmds_.size();
      for (i32 cmd_index = 0; cmd_index < len; ++cmd_index) {
        stale_entries_.emplace(
            &block1, IR::CmdIndex{IR::BlockIndex{block_index}, cmd_index});
      }
    }
  }

  stale_entries_.until_empty([&](const Entry &e) {
    auto &cmd = fn_->Command(e.cmd_index_);
    switch (cmd.op_code_) {
      case IR::Op::UncondJump: return;
      case IR::Op::CondJump: return;
      case IR::Op::ReturnJump: return;
      case IR::Op::SetReturn:
        if (bool *b = std::get_if<bool>(&cmd.args[0].value)) {
          view_.at(e.viewing_block_)
              .view_.at(&fn_->block(e.cmd_index_.block))
              .view_[e.cmd_index_.cmd] =
              std::make_unique<DefaultBoolProperty>(*b);
        }
        break;
      default: NOT_YET(static_cast<int>(cmd.op_code_));
    }
  });
}

// TODO this is not a great way to handle this. Probably should store all
// set-rets first.
void PropertyMap::Returns() const {
  std::vector<IR::CmdIndex> rets;

  // This can be precompeted and stored on the actual IR::Func.
  i32 num_blocks = static_cast<i32>(fn_->blocks_.size());
  for (i32 i = 0; i < num_blocks; ++i) {
    i32 num_cmds = static_cast<i32>(fn_->blocks_[i].cmds_.size());
    for (i32 j = 0; j < num_cmds; ++j) {
      const auto &cmd = fn_->blocks_[i].cmds_[j];
      if (cmd.op_code_ == IR::Op::SetReturn) { rets.push_back(IR::CmdIndex{IR::BlockIndex{i}, j}); }
    }
  }

  // TODO default bool property is way too specifc.
  DefaultBoolProperty acc;
  for (auto ret : rets) {
    IR::BasicBlock *block = &fn_->blocks_[ret.block.value];
    auto *x = view_.at(block).view_.at(block).view_[ret.cmd].get();
    acc.Merge(reinterpret_cast<DefaultBoolProperty *>(x));
  }

  LOG << acc.can_be_true_ << " " << acc.can_be_false_;
}

void DefaultBoolProperty::Merge(const DefaultBoolProperty &p) {
  can_be_true_ &= p.can_be_true_;
  can_be_false_ &= p.can_be_false_;
}
}  // namespace prop
