#include "property/property_map.h"

#include "architecture.h"
#include "ir/func.h"
#include "type/function.h"

namespace prop {
FnStateView::FnStateView(IR::Func *fn) {
  // TODO for now (non-void) registers are counted up from zero to fn->num_regs_
  // - 1 inclusive. This is likely to change.
  for (i32 i = 0; i < static_cast<i32>(fn->num_regs_); ++i) {
    view_.emplace(IR::Register{i}, PropertySet{});
  }
}

PropertyMap::PropertyMap(IR::Func *fn) : fn_(fn) {
  // TODO copy fnstateview rather than creating it repeatedly?
  for (const auto &block : fn_->blocks_) {
    view_.emplace(&block, FnStateView(fn_));
  }

  for (const auto &block1 : fn_->blocks_) {
    for (i32 i = 0; i < static_cast<i32>(fn->num_regs_); ++i) {
      stale_entries_.emplace(&block1, IR::Register{i});
    }
  }
  refresh();
}

void PropertyMap::refresh() {
  stale_entries_.until_empty([&](const Entry &e) {
    auto *cmd_ptr = fn_->Command(e.reg_);
    if (cmd_ptr == nullptr) {
      for (IR::Register reg : fn_->references_.at(e.reg_)) {
        // TODO also this entry on all blocks you jump to
        stale_entries_.emplace(e.viewing_block_, reg);
      }
      return;
    }
    auto &cmd = *cmd_ptr;
    switch (cmd.op_code_) {
      case IR::Op::UncondJump: return;
      case IR::Op::CondJump: return;
      case IR::Op::ReturnJump: return;
      case IR::Op::Not: {
        auto &prop_set = view_.at(e.viewing_block_).view_.at(e.reg_);
        bool change    = false;
        for (auto &prop :
             view_.at(e.viewing_block_).view_.at(cmd.not_.reg_).props_) {
          if (prop->is<DefaultProperty<bool>>()) {
            auto new_prop = std::make_unique<DefaultProperty<bool>>(
                prop->as<DefaultProperty<bool>>());
            std::swap(new_prop->can_be_true_, new_prop->can_be_false_);
            change |= prop_set.add(std::move(new_prop));
          }
        }

        if (change) {
          for (IR::Register reg : fn_->references_.at(e.reg_)) {
            // TODO also this entry on all blocks you jump to
            stale_entries_.emplace(e.viewing_block_, reg);
          }
        }
      } break;
      case IR::Op::SetReturnBool: {
        auto &prop_set = view_.at(e.viewing_block_).view_.at(e.reg_);
        prop_set.add(cmd.set_return_bool_.val_.is_reg_
                         ? std::make_unique<DefaultProperty<bool>>()
                         : std::make_unique<DefaultProperty<bool>>(
                               cmd.set_return_bool_.val_.val_));
      } break;
      default: NOT_YET(static_cast<int>(cmd.op_code_));
    }
  });
}

// TODO this is not a great way to handle this. Probably should store all
// set-rets first.
DefaultProperty<bool> PropertyMap::Returns() const {
  base::vector<IR::CmdIndex> rets;
  base::vector<IR::Register> regs;

  // This can be precompeted and stored on the actual IR::Func.
  i32 num_blocks = static_cast<i32>(fn_->blocks_.size());
  for (i32 i = 0; i < num_blocks; ++i) {
    const auto &block = fn_->blocks_[i];
    i32 num_cmds = static_cast<i32>(block.cmds_.size());
    for (i32 j = 0; j < num_cmds; ++j) {
      const auto &cmd = block.cmds_[j];
      /*
      if (cmd.op_code_ == IR::Op::SetReturn) {
        rets.push_back(IR::CmdIndex{IR::BlockIndex{i}, j});
        regs.push_back(cmd.result);
      }*/
    }
  }

  // TODO default bool property is way too specifc.
  auto acc = DefaultProperty<bool>::Bottom();
  for (size_t i = 0; i < rets.size(); ++i) {
    const auto &ret       = rets[i];
    const auto &reg       = regs[i];
    IR::BasicBlock *block = &fn_->blocks_[ret.block.value];
    const PropertySet& prop_set = view_.at(block).view_.at(reg);
    // TODO blah. not just one entry
    auto *x = prop_set.props_.at(0).get();
    acc.Merge(ASSERT_NOT_NULL(x)->as<DefaultProperty<bool>>());
  }

  return acc;
}

PropertyMap PropertyMap::with_args(const IR::LongArgs &args) const {
  auto copy = *this;
  auto *entry_block = &fn_->block(fn_->entry());
  auto &props       = copy.view_.at(entry_block).view_;

  auto arch = Architecture::InterprettingMachine();
  size_t offset = 0;
  size_t index  = 0;
  while (offset < args.args_.size()) {
    auto *t = args.type_->input.at(index);
    offset  = arch.MoveForwardToAlignment(t, offset);
    if (t == type::Bool) {
      props.at(IR::Register(offset))
          .add(args.is_reg_.at(index)
                   ? std::make_unique<DefaultProperty<bool>>()
                   : std::make_unique<DefaultProperty<bool>>(
                         args.args_.get<bool>(offset)));
      copy.stale_entries_.emplace(entry_block, IR::Register(offset));
    }
    ++index;
    offset += arch.bytes(t);
  }

  for (auto &cmd : entry_block->cmds_) {
    copy.stale_entries_.emplace(entry_block, cmd.result);
  }

  copy.refresh();
  return copy;
}

}  // namespace prop
