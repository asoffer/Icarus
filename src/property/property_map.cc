#include "property/property_map.h"

#include "architecture.h"
#include "ir/func.h"
#include "property/property.h"
#include "type/function.h"

namespace debug {
extern bool validation;
}  // namespace debug

namespace prop {
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

  // TODO all the argument registers.
  for (auto const &block : fn->blocks_) {
    stale_down_.emplace(&block, IR::Register(0));
  }

  for (auto const & [ f, pm ] : fn->preconditions_) {
    auto pm_copy = pm;
    for (const auto &block : f.blocks_) {
      for (const auto &cmd : block.cmds_) {
        if (cmd.op_code_ != IR::Op::SetReturnBool) { continue; }

        // TODO I actually know this on every block.
        auto &pset = pm_copy.view_.at(&block).view_.at(cmd.result);
        pset.add(base::make_owned<prop::BoolProp>(true));
        pm_copy.stale_up_[Entry{&block, cmd.result}].push_back(&pset);
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
    if (!(**prop).is<BoolProp>()) { return; }
    auto p           = &(**prop).as<BoolProp>();
    p->can_be_true_  = !p->can_be_true_;
    p->can_be_false_ = !p->can_be_false_;
  });
  return prop_set;
}

PropertySet NeBool(PropertySet const &lhs, PropertySet const &rhs) {
  return {};  // TODO
}

PropertySet EqBool(PropertySet const &lhs, PropertySet const &rhs) {
  return {};  // TODO
}

}  // namespace

// TODO no longer need to pass stale in as ptr.
void PropertyMap::MarkStale(Entry const &e) {
  for (IR::Register reg : fn_->references_.at(e.reg_)) {
    stale_down_.emplace(e.viewing_block_, reg);
  }

  auto &last_cmd = e.viewing_block_->cmds_.back();
  switch (last_cmd.op_code_) {
    case IR::Op::UncondJump:
      stale_down_.emplace(&fn_->block(last_cmd.uncond_jump_.block_), e.reg_);
      break;
    case IR::Op::CondJump:
      stale_down_.emplace(&fn_->block(last_cmd.cond_jump_.blocks_[0]), e.reg_);
      stale_down_.emplace(&fn_->block(last_cmd.cond_jump_.blocks_[1]), e.reg_);
      break;
    case IR::Op::ReturnJump: break;
    default: UNREACHABLE();
  }
}

static void Debug(PropertyMap const &pm) {
  fprintf(stderr, "\033[2J\033[1;1H\n");  // Clear the screen
  LOG << pm.fn_;
  for (auto const & [ block, view ] : pm.view_) {
    fprintf(stderr, "VIEWING BLOCK: %lu\n", reinterpret_cast<uintptr_t>(block));
    for (auto const & [ reg, prop_set ] : view.view_) {
      fprintf(stderr, "  reg.%d:\n", reg.value);
      for (auto const &p : prop_set.props_) {
        fprintf(stderr, "    %s\n", p->to_string().c_str());
      }
    }
    fprintf(stderr, "\n");
  }
  fgetc(stdin);
}

bool PropertyMap::UpdateEntryFromAbove(Entry const &e) {
  if (debug::validation) { Debug(*this); }

  auto *cmd_ptr = fn_->Command(e.reg_);
  if (cmd_ptr == nullptr) {
    auto inc  = fn_->GetIncomingBlocks();
    auto iter = inc.find(e.viewing_block_);
    if (iter != inc.end()) {
      auto &prop_set = view_.at(e.viewing_block_).view_.at(e.reg_);
      bool changed   = false;
      for (auto const *incoming_block : iter->second) {
        changed |= prop_set.add(view_.at(incoming_block).view_.at(e.reg_));
      }
    }
    return true;
  }

  auto &cmd        = *cmd_ptr;
  auto &block_view = view_.at(e.viewing_block_).view_;
  auto &prop_set   = block_view.at(e.reg_);

  switch (cmd.op_code_) {
    case IR::Op::UncondJump: return /* TODO */ false;
    case IR::Op::CondJump: return /* TODO */ false;
    case IR::Op::ReturnJump: return /* TODO */ false;
    case IR::Op::Call: return /* TODO */ false;
    case IR::Op::Not: return prop_set.add(Not(block_view.at(cmd.not_.reg_)));
    case IR::Op::EqBool:
      return prop_set.add(EqBool(block_view.at(cmd.eq_bool_.args_[0]),
                                 block_view.at(cmd.eq_bool_.args_[1])));
    case IR::Op::NeBool:
      return prop_set.add(NeBool(block_view.at(cmd.ne_bool_.args_[0]),
                                 block_view.at(cmd.ne_bool_.args_[1])));
    case IR::Op::SetReturnBool:
      if (cmd.set_return_bool_.val_.is_reg_) {
        prop_set.add(block_view.at(cmd.set_return_bool_.val_.reg_));
        // TODO Do I need to mark stale upwards?
      } else {
        prop_set.add(
            base::make_owned<BoolProp>(cmd.set_return_bool_.val_.val_));
      }
      return false;
    default: NOT_YET(cmd.op_code_);
  }
}

void PropertyMap::UpdateEntryFromBelow(Entry const &e,
                                       base::vector<PropertySet *> const &ps) {
  if (debug::validation) { Debug(*this); }

  auto *cmd_ptr = fn_->Command(e.reg_);
  if (cmd_ptr == nullptr) {
    auto &prop  = view_.at(e.viewing_block_).view_.at(e.reg_);
    bool change = false;
    for (auto *p : ps) { change |= prop.add(*p); }
    if (change) { stale_down_.insert(e); }
    return;
  }

  auto &cmd = *ASSERT_NOT_NULL(cmd_ptr);
  switch (cmd.op_code_) {
    case IR::Op::SetReturnBool:
      if (cmd.set_return_bool_.val_.is_reg_) {
        stale_up_[Entry{e.viewing_block_, cmd.set_return_bool_.val_.reg_}]
            .push_back(&view_.at(e.viewing_block_).view_.at(e.reg_));
      }
      break;
    case IR::Op::Not: {
      auto &current_prop_set = view_.at(e.viewing_block_).view_.at(e.reg_);
      bool changed           = false;
      for (auto *prop_set : ps) {
        changed |= current_prop_set.add(Not(*prop_set));
      }

      if (changed) {
        stale_up_[Entry{e.viewing_block_, cmd.not_.reg_}].push_back(
            &view_.at(e.viewing_block_).view_.at(e.reg_));
      }
    } break;
    case IR::Op::EqBool: {
      // TODO
    } break;
    case IR::Op::NeBool: {
      // TODO
    } break;
    default: NOT_YET(cmd);
  }
}

void PropertyMap::refresh() {
  do {
    stale_down_.until_empty([this](Entry const &e) {
      if (this->UpdateEntryFromAbove(e)) { MarkStale(e); }
    });
    stale_up_.until_empty(
        [this](Entry const &e, base::vector<PropertySet *> const &p) {
          this->UpdateEntryFromBelow(e, p);
        });
  } while (!stale_down_.empty());
}

// TODO this is not a great way to handle this. Probably should store all
// set-rets first.
BoolProp PropertyMap::Returns() const {
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

  auto bool_ret = BoolProp::Bottom();

  for (size_t i = 0; i < rets.size(); ++i) {
    IR::BasicBlock *block = &fn_->blocks_[rets.at(i).block.value];
    BoolProp acc;
    view_.at(block).view_.at(regs.at(i)).accumulate(&acc);
    bool_ret |= acc;
  }

  return bool_ret;
}

PropertyMap PropertyMap::with_args(IR::LongArgs const &args,
                                   FnStateView const &fn_state_view) const {
  auto copy         = *this;
  auto *entry_block = &fn_->block(fn_->entry());
  auto &props       = copy.view_.at(entry_block).view_;

  auto arch     = Architecture::InterprettingMachine();
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

      // TODO only need to do this on the entry block, but we're not passing
      // info between block views yet.
      for (const auto &b : fn_->blocks_) {
        copy.stale_down_.emplace(&b, IR::Register(offset));
      }
      offset += sizeof(IR::Register);
    } else {
      if (t == type::Bool) {
        props.at(IR::Register(offset))
            .add(base::make_owned<BoolProp>(args.args_.get<bool>(offset)));
        // TODO only need to do this on the entry block, but we're not passing
        // info between block views yet.
        for (const auto &b : fn_->blocks_) {
          copy.stale_down_.emplace(&b, IR::Register(offset));
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
