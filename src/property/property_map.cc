#include "property/property_map.h"

#include "architecture.h"
#include "ir/func.h"
#include "type/function.h"

namespace prop {
// When combining the information available between two properties, we overwrite
// the left-hand side property with any new information gleanable from the
// right-hand side. There are three possible outcomes.
enum class Combine {
  Merge,    // The new property completely subsumes both the left-hand and
            // right-hand sides.
  Partial,  // The new property has more information than the right-hand side,
            // but the right-hand side still has some useful information that
            // isn't expresed in the new property.
  None      // The new property is identical to the left-hand side before
            // combination
};

Combine combine(Property *lhs, Property *rhs) {
  // TODO implement.
  return Combine::None;
}

void PropertySet::add(std::unique_ptr<Property> prop) {
  auto iter        = props_.begin();
  bool saw_partial = true;
  while (saw_partial) {
    saw_partial = false;
    while (iter != props_.end()) {
      switch (combine(prop.get(), iter->get())) {
        case Combine::Merge: props_.erase(iter); break;
        case Combine::Partial:
          saw_partial = true;
          ++iter;
          break;
        case Combine::None: ++iter; break;
      }
    }
  }
  props_.insert(std::move(prop));
}

FnStateView::FnStateView(IR::Func *fn) {
  // TODO this is wrong since registers arent incremented this way.
  for (i32 i = 0; i < static_cast<i32>(fn->num_regs_); ++i) {
    view_.emplace(IR::Register{i}, PropertySet{});
  }

  for (i32 i = -1; i >= -fn->num_voids_; --i) {
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
      case IR::Op::SetReturnBool: {
        auto &prop_set = view_.at(e.viewing_block_).view_.at(e.reg_);
        if (cmd.set_return_bool_.val_.is_reg_) {
          for (const auto &p : view_.at(e.viewing_block_)
                                   .view_.at(cmd.set_return_bool_.val_.reg_)
                                   .props_) {
            auto x = base::wrap_unique(ASSERT_NOT_NULL(p)->Clone());
            prop_set.add(std::move(x));
          }
        } else {
          prop_set.add(std::make_unique<DefaultProperty<bool>>(
              cmd.set_return_bool_.val_.val_));
        }
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
      if (cmd.op_code_ == IR::Op::SetReturnBool) {
        rets.push_back(IR::CmdIndex{IR::BlockIndex{i}, j});
        regs.push_back(cmd.result);
      }
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
    LOG << view_;
    auto *x = prop_set.props_.begin()->get();
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

  // TODO this loop is overkill if you do branching correctly.
  for (const auto & b : fn_->blocks_) {
    // TODO not just register 0. all args
    copy.stale_entries_.emplace(&b, IR::Register{0});
  }

  for (auto &cmd : entry_block->cmds_) {
    copy.stale_entries_.emplace(entry_block, cmd.result);
  }

  copy.refresh();
  return copy;
}

}  // namespace prop
