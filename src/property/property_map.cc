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

  refresh();
}

void PropertyMap::refresh() {
  stale_entries_.until_empty([&](const Entry &e) {
    auto &cmd = fn_->Command(e.cmd_index_);
    switch (cmd.op_code_) {
      case IR::Op::UncondJump: return;
      case IR::Op::CondJump: return;
      case IR::Op::ReturnJump: return;
      case IR::Op::Neg: {
        auto &prop = view_.at(e.viewing_block_)
                         .view_.at(&fn_->block(e.cmd_index_.block))
                         .view_[e.cmd_index_.cmd];
        LOG << prop;
      } break;
      case IR::Op::SetReturn: {
        auto &prop = view_.at(e.viewing_block_)
                         .view_.at(&fn_->block(e.cmd_index_.block))
                         .view_[e.cmd_index_.cmd];

        if (bool *b = std::get_if<bool>(&cmd.args[0].value)) {
          prop = std::make_unique<DefaultProperty<bool>>(*b);

        } else if (IR::Register *reg =
                       std::get_if<IR::Register>(&cmd.args[0].value)) {
          if (cmd.args[0].type == type::Bool) {
            prop = std::make_unique<DefaultProperty<bool>>();
          } else {
            NOT_YET();
          }
        } else {
          LOG << "???";
        }
      } break;
      default: NOT_YET(static_cast<int>(cmd.op_code_));
    }
  });
}

// TODO this is not a great way to handle this. Probably should store all
// set-rets first.
DefaultProperty<bool> PropertyMap::Returns() const {
  std::vector<IR::CmdIndex> rets;

  // This can be precompeted and stored on the actual IR::Func.
  i32 num_blocks = static_cast<i32>(fn_->blocks_.size());
  for (i32 i = 0; i < num_blocks; ++i) {
    i32 num_cmds = static_cast<i32>(fn_->blocks_[i].cmds_.size());
    for (i32 j = 0; j < num_cmds; ++j) {
      const auto &cmd = fn_->blocks_[i].cmds_[j];
      if (cmd.op_code_ == IR::Op::SetReturn) {
        rets.push_back(IR::CmdIndex{IR::BlockIndex{i}, j});
      }
    }
  }

  // TODO default bool property is way too specifc.
  auto acc = DefaultProperty<bool>::Bottom();
  for (auto ret : rets) {
    IR::BasicBlock *block = &fn_->blocks_[ret.block.value];
    auto *x = view_.at(block).view_.at(block).view_[ret.cmd].get();
    acc.Merge(ASSERT_NOT_NULL(x)->as<DefaultProperty<bool>>());
  }

  return acc;
}

PropertyMap PropertyMap::with_args(const std::vector<IR::Val> &args) const {
  auto copy = *this;
  LOG << copy.view_;
  auto *entry_block = &fn_->block(fn_->entry());
  auto& prop_vec = copy.view_.at(entry_block).view_.at(entry_block).view_;
  for (size_t i = 0; i < args.size(); ++i) {
    if (args[i].type == type::Bool) {
      prop_vec.at(i) = base::make_owned<DefaultProperty<bool>>(
          std::get<bool>(args[i].value));
      copy.stale_entries_.emplace(
          entry_block, IR::CmdIndex{IR::BlockIndex{0}, static_cast<i32>(i)});
    }
  }

  copy.refresh();
  LOG << copy.view_;
  return copy;
}

}  // namespace prop
