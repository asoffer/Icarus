#include "property/property_map.h"

#include "ir/arguments.h"
#include "ir/func.h"
#include "layout/arch.h"
#include "property/property.h"
#include "type/function.h"
#include "type/generic_struct.h"

namespace debug {
extern bool validation;
}  // namespace debug

namespace prop {
namespace {
void Debug(PropertyMap const &pm) {
  fprintf(stderr, "\033[2J\033[1;1H\n");  // Clear the screen
  for (auto const & [ block, view ] : pm.view_) {
    fprintf(stderr, "VIEWING BLOCK: %lu\n", reinterpret_cast<uintptr_t>(block));
    for (auto const & [ reg, prop_set ] : view.view_) {
      fprintf(stderr, "  reg.%d:\n", static_cast<int>(reg.value()));
      for (auto const &p : prop_set.props_) {
        fprintf(stderr, "    %s\n", p->to_string().c_str());
      }
    }
    fprintf(stderr, "\n");
  }
  fgetc(stdin);
}

template <typename Fn>
void ForEachArgument(ir::Func const &f, Fn &&fn_to_call) {
  auto arch   = layout::Interpretter();
  auto offset = layout::Bytes{0};

  for (auto *t : f.type_->input) {
    // TODO these register offsets are wrong now that we have compiler_reg_to_offset_.
    offset = layout::FwdAlign(offset, t->alignment(arch));
    fn_to_call(ir::Register{offset.value()});
    offset += t->bytes(arch);
  }
}

template <typename SetContainer, typename Fn>
void until_empty(SetContainer *container, Fn &&fn) {
  while (!container->empty()) {
    auto iter = container->begin();
    while (iter != container->end()) {
      auto next_iter = std::next(iter);
      fn(container->extract(iter).value());
      iter = next_iter;
    }
  }
}

PropertySet Not(PropertySet prop_set) {
  prop_set.props_.for_each([](base::owned_ptr<Property> *prop) {
    if (!(**prop).is<BoolProp>()) { return; }
    auto p           = &(**prop).as<BoolProp>();
    p->can_be_true_  = !p->can_be_true_;
    p->can_be_false_ = !p->can_be_false_;
  });
  return prop_set;
}

PropertySet EqBool(PropertySet const &lhs, PropertySet const &rhs) {
  return {};  // TODO
}

PropertySet LtInt(PropertySet const &lhs, int rhs) {
  auto b = base::make_owned<BoolProp>();
  lhs.props_.for_each([&b, rhs](base::owned_ptr<Property> const *prop) {
    if (!(**prop).is<IntProp>()) { return; }
    auto p = &(**prop).as<IntProp>();

    if (p->lower_) {
      if (p->bound_ >= rhs) { *b = BoolProp(false); }
    } else {
      if (p->bound_ <= rhs) { *b = BoolProp(true); }
    }
  });
  PropertySet ps;
  ps.add(b);
  return ps;
}

}  // namespace

FnStateView::FnStateView(ir::Func *fn) {
  for (auto reg : fn->compiler_reg_to_offset_) {
    view_.emplace(ir::Register(reg), PropertySet{});
  }
}

PropertyMap PropertyMap::AssumingReturnsTrue() const {
  ASSERT(fn_->type_->output.size() == 1u);
  ASSERT(fn_->type_->output.at(0) == type::Bool);
  PropertyMap result = *this;

  std::unordered_set<Entry> stale_up;
  for (auto const &block : fn_->blocks_) {
    for (auto const &cmd : block.cmds_) {
      if (cmd.op_code_ != ir::Op::SetRetBool) { continue; }

      result.lookup(&block, cmd.result)
          .add(base::make_owned<prop::BoolProp>(true));
      stale_up.emplace(&block, cmd.result);
    }
  }

  result.refresh(std::move(stale_up), {});
  return result;
}

PropertyMap::PropertyMap(ir::Func *fn) : fn_(fn) {
  // TODO copy fnstateview rather than creating it repeatedly?
  for (const auto &block : fn_->blocks_) {
    view_.emplace(&block, FnStateView(fn_));
  }

  for (auto const & [ f, pm ] : fn->preconditions_) {
    auto pm_copy = pm.AssumingReturnsTrue();

    ForEachArgument(*fn_, [this, &pm_copy, &f](ir::Register arg) {
      lookup(&fn_->blocks_.at(0), arg)
          .add(pm_copy.lookup(&f.blocks_.at(0), arg));
    });
  }

  std::unordered_set<Entry> stale_down;
  auto *entry_block = &fn_->block(fn_->entry());
  ForEachArgument(*fn_, [&stale_down, entry_block](ir::Register arg) {
    stale_down.emplace(entry_block, ir::Register(0));
  });

  // This refresh is an optimization. Because it's likely that this gets called
  // many times with different arguments, it's better to precompute whatever can
  // be on this function rather than repeating all that on many different calls.
  refresh({}, std::move(stale_down));
}

// TODO no longer need to pass stale in as ptr.
void PropertyMap::MarkReferencesStale(Entry const &e,
                                      std::unordered_set<Entry> *stale_down) {
  for (ir::Register reg : fn_->references_.at(e.reg_)) {
    stale_down->emplace(e.viewing_block_, reg);
  }

  auto &last_cmd = e.viewing_block_->cmds_.back();
  switch (last_cmd.op_code_) {
    case ir::Op::UncondJump:
      stale_down->emplace(&fn_->block(last_cmd.block_), e.reg_);
      break;
    case ir::Op::CondJump:
      stale_down->emplace(&fn_->block(last_cmd.cond_jump_.blocks_[0]), e.reg_);
      stale_down->emplace(&fn_->block(last_cmd.cond_jump_.blocks_[1]), e.reg_);
      break;
    case ir::Op::ReturnJump: break;
    default: UNREACHABLE();
  }
}

bool PropertyMap::UpdateEntryFromAbove(Entry const &e) {
  if (debug::validation) { Debug(*this); }

  auto *cmd_ptr = fn_->Command(e.reg_);
  if (cmd_ptr == nullptr) {
    auto inc  = fn_->GetIncomingBlocks();
    auto iter = inc.find(e.viewing_block_);
    if (iter != inc.end()) {
      auto &prop_set = this->lookup(e);
      bool changed   = false;
      for (auto const *incoming_block : iter->second) {
        changed |= prop_set.add(this->lookup(incoming_block, e.reg_));
      }
    }
    return true;
  }

  auto &cmd        = *cmd_ptr;
  auto &block_view = view_.at(e.viewing_block_).view_;
  auto &prop_set   = block_view.at(e.reg_);

  switch (cmd.op_code_) {
    case ir::Op::UncondJump: return /* TODO */ false;
    case ir::Op::CondJump: return /* TODO */ false;
    case ir::Op::ReturnJump: return /* TODO */ false;
    case ir::Op::Call: return /* TODO */ false;
    case ir::Op::NotBool: return prop_set.add(Not(block_view.at(cmd.reg_)));
    case ir::Op::EqBool:
      return prop_set.add(EqBool(block_view.at(cmd.bool_args_.args_[0].reg_),
                                 block_view.at(cmd.bool_args_.args_[1].reg_)));
    case ir::Op::LtInt32:
      if (cmd.i32_args_.args_[0].is_reg_) {
        if (cmd.i32_args_.args_[1].is_reg_) {
          NOT_YET();
        } else {
          return prop_set.add(LtInt(block_view.at(cmd.i32_args_.args_[0].reg_),
                                    cmd.i32_args_.args_[1].val_));
        }
      } else {
        NOT_YET();
      }

    case ir::Op::SetRetBool:
      if (cmd.set_ret_bool_.val_.is_reg_) {
        prop_set.add(block_view.at(cmd.set_ret_bool_.val_.reg_));
        // TODO Do I need to mark stale upwards?
      } else {
        prop_set.add(
            base::make_owned<BoolProp>(cmd.set_ret_bool_.val_.val_));
      }
      return false;
    default: NOT_YET(cmd.op_code_);
  }
}

void PropertyMap::UpdateEntryFromBelow(Entry const &e,
                                       std::unordered_set<Entry> *stale_up,
                                       std::unordered_set<Entry> *stale_down) {
  if (debug::validation) { Debug(*this); }

  auto *cmd_ptr = fn_->Command(e.reg_);
  if (cmd_ptr == nullptr) {
    stale_down->insert(e);
    return;
  }
  auto &cmd  = *ASSERT_NOT_NULL(cmd_ptr);
  auto &view = view_.at(e.viewing_block_).view_;
  switch (cmd.op_code_) {
#define DEFINE_CASE(op_name)                                                   \
  {                                                                            \
    if (cmd.op_name.val_.is_reg_) {                                            \
      view.at(cmd.op_name.val_.reg_).add(view.at(e.reg_));                     \
      stale_up->emplace(e.viewing_block_, cmd.op_name.val_.reg_);              \
    }                                                                          \
  }                                                                            \
  break
    case ir::Op::SetRetBool: DEFINE_CASE(set_ret_bool_);
    case ir::Op::SetRetInt32: DEFINE_CASE(set_ret_i32_);
    case ir::Op::SetRetFloat32: DEFINE_CASE(set_ret_float32_);
    case ir::Op::SetRetFloat64: DEFINE_CASE(set_ret_float64_);
    case ir::Op::SetRetType: DEFINE_CASE(set_ret_type_);
    case ir::Op::SetRetEnum: DEFINE_CASE(set_ret_enum_);
    case ir::Op::SetRetByteView: DEFINE_CASE(set_ret_byte_view_);
    case ir::Op::SetRetFlags: DEFINE_CASE(set_ret_flags_);
    case ir::Op::SetRetAddr: DEFINE_CASE(set_ret_addr_);
    case ir::Op::SetRetFunc: DEFINE_CASE(set_ret_func_);
    case ir::Op::SetRetScope: DEFINE_CASE(set_ret_scope_);
    case ir::Op::SetRetModule: DEFINE_CASE(set_ret_module_);
    case ir::Op::SetRetBlock: DEFINE_CASE(set_ret_block_);
#undef DEFINE_CASE
    case ir::Op::NotBool: {
      // Not works in both directions. Huzzah!
      bool changed = view.at(cmd.reg_).add(Not(view.at(e.reg_)));
      if (changed) { stale_up->emplace(e.viewing_block_, cmd.reg_); }
    } break;
                              /*
    case ir::Op::LtInt: {
      auto &prop_set = view.at(e.reg_).props_;
      prop_set.for_each([&](base::owned_ptr<Property> *prop) {
        if (!(**prop).is<BoolProp>()) { return; }
        auto &bool_prop = (**prop).as<BoolProp>();
        if (bool_prop.can_be_false_ && bool_prop.can_be_true_) { return; }
        auto[reg, int_prop] =
            IntProp::Make(cmd.i32_args_, !bool_prop.can_be_false_);
        bool changed = view.at(reg).add(std::move(int_prop));
        if (changed) { stale_up->emplace(e.viewing_block_, reg); }
      });
    } break;
    case ir::Op::LeInt: {
      auto &prop_set = view.at(e.reg_).props_;
      prop_set.for_each([&](base::owned_ptr<Property> *prop) {
        if (!(**prop).is<BoolProp>()) { return; }
        auto &bool_prop = (**prop).as<BoolProp>();
        if (bool_prop.can_be_false_ && bool_prop.can_be_true_) { return; }
        auto[reg, int_prop] =
            IntProp::Make(cmd.i32_args_, !bool_prop.can_be_false_);
        bool changed = view.at(reg).add(std::move(int_prop));
        if (changed) { stale_up->emplace(e.viewing_block_, reg); }
      });
    } break;
    case ir::Op::GtInt: {
      auto &prop_set = view.at(e.reg_).props_;
      prop_set.for_each([&](base::owned_ptr<Property> *prop) {
        if (!(**prop).is<BoolProp>()) { return; }
        auto &bool_prop = (**prop).as<BoolProp>();
        if (bool_prop.can_be_false_ && bool_prop.can_be_true_) { return; }
        auto[reg, int_prop] =
            IntProp::Make(cmd.i32_args_, !bool_prop.can_be_false_);
        bool changed = view.at(reg).add(std::move(int_prop));
        if (changed) { stale_up->emplace(e.viewing_block_, reg); }
      });
    } break;
    case ir::Op::GeInt: {
      auto &prop_set = view.at(e.reg_).props_;
      prop_set.for_each([&](base::owned_ptr<Property> *prop) {
        if (!(**prop).is<BoolProp>()) { return; }
        auto &bool_prop = (**prop).as<BoolProp>();
        if (bool_prop.can_be_false_ && bool_prop.can_be_true_) { return; }
        auto[reg, int_prop] =
            IntProp::Make(cmd.i32_args_, !bool_prop.can_be_false_);
        bool changed = view.at(reg).add(std::move(int_prop));
        if (changed) { stale_up->emplace(e.viewing_block_, reg); }
      });
    } break;
    */
    default: NOT_YET(cmd);
  }
}

void PropertyMap::refresh(std::unordered_set<Entry> stale_up,
                          std::unordered_set<Entry> stale_down) {
  do {
    until_empty(&stale_down, [this, &stale_down](Entry const &e) {
      if (this->UpdateEntryFromAbove(e)) {
        MarkReferencesStale(e, &stale_down);
      }
    });
    until_empty(&stale_up, [this, &stale_up, &stale_down](Entry const &e) {
      this->UpdateEntryFromBelow(e, &stale_up, &stale_down);
    });
  } while (!stale_down.empty());
}

// TODO this is not a great way to handle this. Probably should store all
// set-rets first.
BoolProp PropertyMap::Returns() const {
  std::vector<ir::CmdIndex> rets;
  std::vector<ir::Register> regs;

  // This can be precompeted and stored on the actual ir::Func.
  int32_t num_blocks = static_cast<int32_t>(fn_->blocks_.size());
  for (int32_t i = 0; i < num_blocks; ++i) {
    const auto &block = fn_->blocks_[i];
    int32_t num_cmds      = static_cast<int32_t>(block.cmds_.size());
    for (int32_t j = 0; j < num_cmds; ++j) {
      const auto &cmd = block.cmds_[j];
      if (cmd.op_code_ == ir::Op::SetRetBool) {
        rets.push_back(ir::CmdIndex{ir::BlockIndex{i}, j});
        regs.push_back(cmd.result);
      }
    }
  }

  auto bool_ret = BoolProp::Bottom();

  for (size_t i = 0; i < rets.size(); ++i) {
    ir::BasicBlock *block = &fn_->blocks_[rets.at(i).block.value];
    BoolProp acc;
    lookup(block, regs.at(i)).accumulate(&acc);
    bool_ret |= acc;
  }

  return bool_ret;
}

PropertyMap PropertyMap::with_args(ir::Arguments const &args,
                                   FnStateView const &fn_state_view) const {
  auto copy         = *this;
  auto *entry_block = &fn_->block(fn_->entry());
  auto &props       = copy.view_.at(entry_block).view_;

  auto arch    = layout::Interpretter();
  auto offset  = layout::Bytes{0};
  size_t index = 0;
  // TODO offset < args.args_.size() should work as the condition but it isn't,
  // so figure out why.

  std::unordered_set<Entry> stale_down;
  std::vector<type::Type const *> const &ins = [&] {
    if (args.type_->is<type::Function>()) {
      return args.type_->as<type::Function>().input;
    } else if (args.type_->is<type::GenericStruct>()) {
      return args.type_->as<type::GenericStruct>().deps_;
    } else {
      UNREACHABLE();
    }
  }();

  while (index < ins.size()) {
    auto *t = ins.at(index);
    offset = layout::FwdAlign(offset, t->alignment(arch));
    // TODO instead of looking for non-register args, this should be moved out
    // to the caller. because registers might also have some properties that can
    // be reasoned about, all of this should be figured out where it's known and
    // then passed in.
    if (args.results_.is_reg(index)) {
      props.at(ir::Register(offset.value()))
          .add(fn_state_view.view_.at(args.results_.get<ir::Register>(index)));

      // TODO only need to do this on the entry block, but we're not passing
      // info between block views yet.
      for (const auto &b : fn_->blocks_) {
        // TODO Pretty sure this is wrong now that we have compiler_reg_to_offset_
        stale_down.emplace(&b, ir::Register(offset.value()));
      }
      offset += layout::Bytes{sizeof(ir::Register)};
    } else {
      if (t == type::Bool) {
        props.at(ir::Register(offset.value()))
            .add(base::make_owned<BoolProp>(
                args.results_.get<bool>(offset.value()).val_));
        // TODO only need to do this on the entry block, but we're not passing
        // info between block views yet.
        for (const auto &b : fn_->blocks_) {
          stale_down.emplace(&b, ir::Register(offset.value()));
        }
      }
      offset += t->bytes(arch);
    }
    ++index;
  }

  copy.refresh({}, std::move(stale_down));
  return copy;
}

}  // namespace prop
