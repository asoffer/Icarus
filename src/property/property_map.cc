#include "property/property_map.h"

#include "architecture.h"
#include "ir/func.h"
#include "type/function.h"

namespace prop {
// When combining the information available between two properties, we overwrite
// the left-hand side property with any new information gleanable from the
// right-hand side. There are several possible outcomes.
enum class Combine {
  Same,     // The properties are equal
  Merge,    // The new property completely subsumes both the left-hand and
            // right-hand sides.
  Partial,  // The new property has more information than the right-hand side,
            // but the right-hand side still has some useful information that
            // isn't expresed in the new property.
  None      // The new property is identical to the left-hand side before
            // combination
};

Combine combine(Property *lhs, Property const *rhs) {
  if (lhs->is<DefaultProperty<bool>>() && rhs->is<DefaultProperty<bool>>()) {
    auto& l = lhs->as<DefaultProperty<bool>>();
    auto& r = rhs->as<DefaultProperty<bool>>();
    auto prev = l;
    if (l.can_be_true_ == r.can_be_true_ &&
        l.can_be_false_ == r.can_be_false_) {
      return Combine::Same;
    }

    l.can_be_true_ &= r.can_be_true_;
    l.can_be_false_ &= r.can_be_false_;
    return (l.can_be_true_ == prev.can_be_true_ &&
            l.can_be_false_ == prev.can_be_false_)
               ? Combine::None
               : Combine::Merge;
  }
  // TODO handling all pairs is not scalable (or even possible with user-defined
  // properties.
  return Combine::None;
}

bool PropertySet::add(base::owned_ptr<Property> prop) {
  if (props_.empty()) {
    props_.insert(std::move(prop));
    return true;
  }

  bool change = false;
  bool saw_partial = true;
  while (saw_partial) {
    saw_partial = false;
    auto iter   = props_.begin();
    // TODO write down the exact invariants that this guarantees. Something like
    // "every pair of properties is incomparbale."
    while (iter != props_.end()) {
      switch (combine(prop.get(), iter->get())) {
        case Combine::Same: return false;
        case Combine::Merge:
          change = true;
          props_.erase(iter);
          break;
        case Combine::Partial:
          saw_partial = true;
          change      = true;
          ++iter;
          break;
        case Combine::None: ++iter; break;
      }
    }
  }
  props_.insert(std::move(prop));
  return change;
}

bool PropertySet::add(const PropertySet &prop_set) {
  bool change = false;
  for (auto const &p : prop_set.props_) { change |= add(p); }
  return change;
}

void PropertySet::accumulate(Property *prop) const {
  ASSERT(prop != nullptr);
  bool changed = true;
  while (changed) {
    changed = false;
    for (const auto &p : props_) {
      auto c = combine(prop, p.get());
      switch (c) {
        case Combine::Merge:
        case Combine::Partial: changed = true;
        default:;
      }
    }
  }
}

FnStateView::FnStateView(IR::Func *fn) {
  for (const auto & [ num, reg ] : fn->reg_map_) {
    view_.emplace(reg, PropertySet{});
  }
}

PropertyMap::PropertyMap(IR::Func *fn) : fn_(fn) {
  // TODO copy fnstateview rather than creating it repeatedly?
  for (const auto &block : fn_->blocks_) {
    view_.emplace(&block, FnStateView(fn_));
  }

  // TODO this is overkill, but you do need to consider blocks not reachable
  // from the function arguments.
  for (const auto &block1 : fn_->blocks_) {
    for (const auto & [ num, reg ] : fn->reg_map_) {
      stale_entries_.emplace(&block1, reg);
    }
  }

  for (auto const & [ f, pm ] : fn->preconditions_) {
    auto pm_copy = pm;
    for (const auto &block : f.blocks_) {
      for (const auto &cmd : block.cmds_) {
        if (cmd.op_code_ != IR::Op::SetReturnBool) { continue; }

        // TODO I actually know this on every block.
        pm_copy.view_.at(&block)
            .view_.at(cmd.result)
            .add(base::make_owned<prop::DefaultProperty<bool>>(true));
      }
    }
    pm_copy.refresh();

    // TODO all the registers
    this->view_.at(&fn_->blocks_.at(0))
        .view_.at(IR::Register(0))
        .add(pm_copy.view_.at(&f.blocks_.at(0)).view_.at(IR::Register(0)));
  }

  // This refresh is an optimization. Because it's likely that this gets called
  // many times with different arguments, it's better to precompute whatever can
  // be on this function rather than repeating all that on many different calls.
  refresh();
}

namespace {
PropertySet Not(PropertySet prop_set) {
  prop_set.props_.for_each([](base::owned_ptr<Property> *prop) {
    if (!(**prop).is<DefaultProperty<bool>>()) { return; }
    auto p           = &(**prop).as<DefaultProperty<bool>>();
    p->can_be_true_  = !p->can_be_true_;
    p->can_be_false_ = !p->can_be_false_;
  });
  return prop_set;
}
}  // namespace

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
    auto &cmd        = *cmd_ptr;
    auto &block_view = view_.at(e.viewing_block_).view_;
    auto &prop_set   = block_view.at(e.reg_);

    bool change = false;
    switch (cmd.op_code_) {
      case IR::Op::UncondJump:
        return;  // TODO
      case IR::Op::CondJump:
        return;  // TODO
      case IR::Op::ReturnJump:
        return;  // TODO
      case IR::Op::Call:
        return;  // TODO
      case IR::Op::Not:
        change = prop_set.add(Not(block_view.at(cmd.not_.reg_)));
        if (change) { stale_entries_.emplace(e.viewing_block_, cmd.not_.reg_); }
        break;
      case IR::Op::SetReturnBool: {
        if (cmd.set_return_bool_.val_.is_reg_) {
          change = prop_set.add(block_view.at(cmd.set_return_bool_.val_.reg_));
          if (change) {
            stale_entries_.emplace(e.viewing_block_,
                                   cmd.set_return_bool_.val_.reg_);
          }
        } else {
          // TODO Adding a default property seems really silly. You should be
          // able to just not add anything.
          prop_set.add(base::make_owned<DefaultProperty<bool>>(
              cmd.set_return_bool_.val_.val_));
        }
      } break;
      default: NOT_YET(IR::OpCodeStr(cmd.op_code_);
    }
    if (change) {
      for (IR::Register reg : fn_->references_.at(cmd.result)) {
        stale_entries_.emplace(e.viewing_block_, reg);
      }
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
    i32 num_cmds      = static_cast<i32>(block.cmds_.size());
    for (i32 j = 0; j < num_cmds; ++j) {
      const auto &cmd = block.cmds_[j];
      if (cmd.op_code_ == IR::Op::SetReturnBool) {
        rets.push_back(IR::CmdIndex{IR::BlockIndex{i}, j});
        regs.push_back(cmd.result);
      }
    }
  }

  auto bool_ret = DefaultProperty<bool>::Bottom();

  for (size_t i = 0; i < rets.size(); ++i) {
    IR::BasicBlock *block = &fn_->blocks_[rets.at(i).block.value];
    DefaultProperty<bool> acc;
    view_.at(block).view_.at(regs.at(i)).accumulate(&acc);
    bool_ret |= acc;
  }

  return bool_ret;
}

PropertyMap PropertyMap::with_args(IR::LongArgs const &args,
                                   FnStateView const &fn_state_view) const {
  auto copy = *this;
  auto *entry_block = &fn_->block(fn_->entry());
  auto &props       = copy.view_.at(entry_block).view_;

  auto arch = Architecture::InterprettingMachine();
  size_t offset = 0;
  size_t index  = 0;
  // TODO offset < args.args_.size() should work as the condition but it isn't,
  // so figure out why.
  while (index < args.type_->input.size()) {
    auto *t = args.type_->input.at(index);
    offset  = arch.MoveForwardToAlignment(t, offset);
    // TODO instead of looking for non-register args, this should be moved out
    // to the caller. because registers might also have some properties that can
    // be reasoned about, all of this should be figured out where it's known and
    // then passed in.
    if (args.is_reg_.at(index)) {
      props.at(IR::Register(offset))
          .add(fn_state_view.view_.at(args.args_.get<IR::Register>(offset)));
      offset += sizeof(IR::Register);
    } else {
      if (t == type::Bool) {
        props.at(IR::Register(offset))
            .add(base::make_owned<DefaultProperty<bool>>(
                args.args_.get<bool>(offset)));
        // TODO only need to do this on the entry block, but we're not passing
        // info between block views yet.
        for (const auto &b : fn_->blocks_) {
          copy.stale_entries_.emplace(&b, IR::Register(offset));
        }
      }
    offset += arch.bytes(t);
    }
    ++index;
  }

  copy.refresh();
  return copy;
}

}  // namespace prop
