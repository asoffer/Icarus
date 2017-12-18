#include "ir.h"

#include "../error_log.h"
#include "property.h"

namespace debug {
extern bool no_validation;
} // namespace debug

namespace IR {
namespace property {
struct PropView {
  std::unordered_map<Register, base::owned_ptr<Property>> props_;
  std::unordered_map<ReturnValue, base::owned_ptr<Property>> ret_props_;
  std::unordered_map<const Block *, std::unordered_set<const Block *>>
      incoming_;
  std::unordered_set<const Block *> unreachable_;
};

static base::owned_ptr<Property> DefaultProperty(Type* t) {
  if (t == Bool) { return base::make_owned<BoolProperty>(); }
  if (t == Int) { return base::make_owned<Range<i32>>(); }
  return nullptr;
}

struct PropDB {
  PropDB(const Func *fn) : fn_(fn) {
    // Build a generic reachability set once and copy it for each view.
    // TODO copy-on-write
    std::unordered_map<const Block *, std::unordered_set<const Block *>>
        incoming;
    for (const auto &block : fn_->blocks_) {
      const auto &last_cmd = block.cmds_.back();
      switch (last_cmd.op_code_) {
      case Op::UncondJump:
        incoming[&fn_->block(std::get<BlockIndex>(last_cmd.args[0].value))]
            .insert(&block);
        break;
      case Op::CondJump:
        incoming[&fn_->block(std::get<BlockIndex>(last_cmd.args[1].value))]
            .insert(&block);
        incoming[&fn_->block(std::get<BlockIndex>(last_cmd.args[2].value))]
            .insert(&block);
        break;
      case Op::ReturnJump: /* Nothing to do */ break;
      default: fn_->dump(); UNREACHABLE(static_cast<int>(last_cmd.op_code_));
      }
    }
    // Hack: First entry depends on itself.
    incoming[&fn_->block(fn_->entry())].insert(&fn_->block(fn_->entry()));

    for (const auto &viewing_block : fn_->blocks_) {
      auto &view = views_[&viewing_block];
      if (fn_->args_.size() == 1) {
        auto prop = DefaultProperty(fn_->type->input);
        ASSERT(prop.get() != nullptr, "");
        view.props_[Register(0)] = std::move(prop);
      } else if (fn_->args_.size() > 1) {
        for (i32 i = 0; i < static_cast<i32>(fn_->args_.size()); ++i) {
          Type *entry_type = ptr_cast<Tuple>(fn_->type->input)->entries[i];
          auto prop        = DefaultProperty(entry_type);
          ASSERT(prop.get() != nullptr, "");
          view.props_[Register(i)] = std::move(prop);
        }
      }
      for (const auto &block : fn_->blocks_) {
        for (const auto &cmd : block.cmds_) {
          auto prop = DefaultProperty(cmd.type);
          if (prop.get() != nullptr) {
            view.props_[cmd.result] = std::move(prop);
          }
        }
      }
      view.incoming_ = incoming;

      if (fn_->type->output->is<Tuple>()) {
        i32 i = 0;
        for (Type *entry : fn_->type->output->as<Tuple>().entries) {
          view.ret_props_[ReturnValue(i)] = DefaultProperty(entry);
          ++i;
        }
      } else if (fn_->type->output != Void) {
        view.ret_props_[ReturnValue(0)] = DefaultProperty(fn_->type->output);
      }
    }

    // Only mark things on the entry view and entry block as stale to begin
    // with. All else can be propogated.
    const auto& entry_block = fn_->blocks_[0];
    for (size_t i = 0; i < entry_block.cmds_.size(); ++i) {
      const auto& cmd = entry_block.cmds_[i];
      if (cmd.type == nullptr || cmd.type == Void) { continue; }
      MarkReferencesStale(entry_block, CmdIndex{0, static_cast<i32>(i)});
    }
  }

  PropDB(PropDB &&) = default;

  void MarkReferencesStale(const Block &block, CmdIndex cmd_index) {
    {
      auto iter = fn_->references_.find(cmd_index);
      if (iter != fn_->references_.end()) {
        const auto &references = iter->second;

        for (const auto &cmd_index : references) {
          stale_.emplace(&block, cmd_index);
        }
      }
    }
    const auto &last_cmd = block.cmds_.back();
    switch (last_cmd.op_code_) {
    case Op::UncondJump:
      stale_.emplace(&fn_->block(std::get<BlockIndex>(last_cmd.args[0].value)),
                     cmd_index);

      break;
    case Op::CondJump:
      stale_.emplace(&fn_->block(std::get<BlockIndex>(last_cmd.args[1].value)),
                     cmd_index);
      stale_.emplace(&fn_->block(std::get<BlockIndex>(last_cmd.args[2].value)),
                     cmd_index);
      break;
    case Op::ReturnJump: /* Nothing to do */ break;
    default: UNREACHABLE();
    }
  }

  static PropDB Make(const Func *fn,
                     std::vector<base::owned_ptr<Property>> args,
                     std::queue<Func *> *validation_queue,
                     std::vector<std::pair<const Block *, const Cmd *>> *calls);

  base::owned_ptr<Property> Get(const Block &block, Register reg) {
    return views_[&block].props_[reg];
  }

  base::owned_ptr<Property> GetShadow(const Block &block, CmdIndex cmd_index) {
    // TODO you always know if it's SetReturn at compile-time so you should pull
    // these into two separate functions
    const auto& cmd = fn_->Command(cmd_index);
    Type *type = (cmd.op_code_ == Op::SetReturn) ? cmd.args[1].type : cmd.type;
    const auto &incoming_set =
        views_[&block].incoming_[&fn_->block(cmd_index.block)];

    if (type == Bool) {
      std::vector<BoolProperty> props;
      props.reserve(incoming_set.size());
      for (const Block *incoming_block : incoming_set) {
        props.push_back(
            Get(*incoming_block, (cmd.op_code_ == Op::SetReturn)
                                     ? std::get<Register>(cmd.args[1].value)
                                     : cmd.result)
                ->as<BoolProperty>());
      }

      return BoolProperty::WeakMerge(props);
    } else if (type == Int) {
      std::vector<Range<i32>> props;
      props.reserve(incoming_set.size());
      for (const Block *incoming_block : incoming_set) {
        if (cmd.op_code_ == Op::SetReturn) {
          props.push_back(Get(*incoming_block, cmd.args[1])->as<Range<i32>>());
        } else {
          props.push_back(Get(*incoming_block, cmd.result)->as<Range<i32>>());
        }
      }
      return Range<i32>::WeakMerge(props);
    } else {
      NOT_YET();
    }
  }

  void Set(const Block &block, CmdIndex cmd_index,
           base::owned_ptr<Property> new_prop) {
    const auto &cmd = fn_->Command(cmd_index);
    Type *type = (cmd.op_code_ == Op::SetReturn) ? cmd.args[1].type : cmd.type;
    // TODO you keep checking and forgetting the types here :(
    if (type == Bool) {
      new_prop = BoolProperty::StrongMerge(
          new_prop->as<BoolProperty>(),
          GetShadow(block, cmd_index)->as<BoolProperty>());
    } else if (type == Int) {
      new_prop = Range<i32>::StrongMerge(
          new_prop->as<Range<i32>>(),
          GetShadow(block, cmd_index)->as<Range<i32>>());
    } else {
      NOT_YET();
    }

    auto &old_prop =
        (cmd.op_code_ == Op::SetReturn)
            ? views_[&block]
                  .ret_props_[std::get<ReturnValue>(cmd.args[0].value)]
            : views_[&block].props_[cmd.result];
    if (!old_prop->Implies(*new_prop)) {
      old_prop = std::move(new_prop);
      MarkReferencesStale(block, cmd_index);
    }
  }

  void SetUnreachable(const Block &view, const Block &from, const Block &to) {
    // TODO clean up stales still referencing this blocks.
    auto &reach_set = views_[&view].incoming_[&to];
    reach_set.erase(&from);
    CmdIndex last_cmd_index{static_cast<i32>(&to - fn_->blocks_.data()),
                            static_cast<i32>(to.cmds_.size() - 1)};
    const auto &last_cmd = to.cmds_.back();
    if (reach_set.empty()) {
      views_[&view].unreachable_.insert(&to);

      // TODO What about weak back-references? We don't yet handle loops.
      switch (last_cmd.op_code_) {
      case Op::UncondJump:
        SetUnreachable(
            view, to, fn_->block(std::get<BlockIndex>(last_cmd.args[0].value)));
        break;
      case Op::CondJump:
        SetUnreachable(
            view, to, fn_->block(std::get<BlockIndex>(last_cmd.args[1].value)));
        SetUnreachable(
            view, to, fn_->block(std::get<BlockIndex>(last_cmd.args[2].value)));

        break;
      case Op::ReturnJump: /* Nothing to do */ break;
      default: UNREACHABLE();
      }
    }
    MarkReferencesStale(view, last_cmd_index);
  }

  bool Reachable(const Block &view, const Block &from, const Block &to) const {
    auto prop_view_iter = views_.find(&view);
    ASSERT(prop_view_iter != views_.end(), "");

    const auto &incoming_map  = prop_view_iter->second.incoming_;
    auto incoming_set_iter = incoming_map.find(&to);
    ASSERT(incoming_set_iter != incoming_map.end(), "");
    const auto &incoming_set = incoming_set_iter->second;
    bool result = incoming_set.find(&from) != incoming_set.end();
    return result;
  }

  base::owned_ptr<Property> Get(const Block &block, Val arg) {
    return std::visit(
        base::overloaded{
            [&block, this](Register r) -> base::owned_ptr<Property> {
              return this->Get(block, r);
            },
            [](i32 n) -> base::owned_ptr<Property> {
              return base::make_owned<Range<i32>>(n, n);
            },
            [](bool b) -> base::owned_ptr<Property> {
              return base::make_owned<BoolProperty>(b);
            },
            [&arg](auto) -> base::owned_ptr<Property> { NOT_YET(arg); },
        },
        arg.value);
  }

  base::owned_ptr<BoolProperty> GetBoolProp(const Block &block, Val arg) {
    return std::visit(
        base::overloaded{
            [&block, this](Register r) -> base::owned_ptr<BoolProperty> {
              return Get(block, r).as<BoolProperty>();
            },
            [](bool b) -> base::owned_ptr<BoolProperty> {
              return base::make_owned<BoolProperty>(b);
            },
            [&arg](auto) -> base::owned_ptr<BoolProperty> { UNREACHABLE(arg); },
        },
        arg.value);
  }

  void Compute();

  const Func *fn_;
  std::unordered_map<const Block *, PropView> views_;
  // TODO should be an unordered_set but I haven't define an appropriate hash
  // function for the pair yet.
  std::set<std::pair<const Block *, CmdIndex>> stale_;
  // TODO should be per block
};

void PropDB::Compute() {
  while (!stale_.empty()) {
    auto iter                = stale_.begin();
    const Block *block       = iter->first;
    const CmdIndex cmd_index = iter->second;

    if (cmd_index.cmd < 0) {
      // Is register!
      // 
      // TODO arguments can be stale: viewed from a different block you might
      // condition on them having some property and want to use that in
      // verification. You need to handle this properly which likely just
      // entails merging from incoming blocks.
      stale_.erase(iter);
      continue;
    }

    const Cmd& cmd = fn_->Command(cmd_index);

    // TODO optimize. Lots of redundant lookups here.
    const Block& cmd_block = fn_->block(cmd_index.block);
    const auto& unreachable = views_[block].unreachable_;
    if (unreachable.find(&cmd_block) != unreachable.end()) {
      // No need to look at anything on a block that's not reachable
      stale_.erase(iter);
      continue;
    }

    auto prop = Get(*block, cmd.result);
    stale_.erase(iter);

    switch (cmd.op_code_) {
    case Op::Neg: {
      auto prop = Get(*block, cmd.args[0]);
      prop->Neg();
      Set(*block, cmd_index, std::move(prop));
    } break;
#define CASE(case_name, case_sym)                                              \
  case Op::case_name: {                                                        \
    Set(*block, cmd_index,                                                     \
        Get(*block, cmd.args[0]) case_sym Get(*block, cmd.args[1]));           \
  } break
      CASE(Add, +);
      CASE(Sub, -);
      CASE(Mul, *);
      CASE(Lt, <);
      CASE(Le, <=);
      CASE(Ge, >=);
      CASE(Gt, >);
      CASE(Xor, ^);
#undef CASE
    case Op::Phi: {
      if (cmd.type == Bool) {
        std::vector<BoolProperty> props;
        for (size_t i = 0; i < cmd.args.size(); i += 2) {
          if (Reachable(
                  /* view = */ *block,
                  /* from = */ fn_->block(
                      std::get<BlockIndex>(cmd.args[i].value)),
                  /*   to = */ cmd_block)) {
            props.push_back(*GetBoolProp(*block, cmd.args[i + 1]));
          }
        }

        ASSERT(!props.empty(), "No longer reachable");
        // TODO this is not a great way to do merging because it's not genric,
        // but it's simple for now.
        Set(*block, cmd_index, BoolProperty::WeakMerge(props));
      } else {
        NOT_YET();
      }
    } break;
    case Op::Print: break;
    case Op::Call:
      // TODO No post-conditions yet, so nothing to see here.
      continue;
    case Op::Nop: break;
    case Op::SetReturn: Set(*block, cmd_index, Get(*block, cmd.args[1])); break;
    case Op::CondJump: {
      auto prop       = Get(*block, std::get<Register>(cmd.args[0].value));
      auto *bool_prop = dynamic_cast<BoolProperty *>(prop.get());
      if (bool_prop == nullptr) { continue; }
      auto &true_block  = fn_->block(std::get<BlockIndex>(cmd.args[1].value));
      auto &false_block = fn_->block(std::get<BlockIndex>(cmd.args[2].value));

      if (bool_prop->kind == BoolProperty::Kind::True) {
        SetUnreachable(/* view = */ *block,
                       /* from = */ cmd_block,
                       /*   to = */ false_block);
      } else if (bool_prop->kind == BoolProperty::Kind::False) {
        SetUnreachable(/* view = */ *block,
                       /* from = */ cmd_block,
                       /*   to = */ true_block);
      }
    } break;
    case Op::UncondJump: /* Nothing to do */ continue;
    case Op::ReturnJump: /* Nothing to do */ continue;

    default:
      LOG << "Not yet handled: " << static_cast<int>(cmd.op_code_);
      continue;
    }
  }
}

PropDB PropDB::Make(const Func *fn, std::vector<base::owned_ptr<Property>> args,
                    std::queue<Func *> *validation_queue,
                    std::vector<std::pair<const Block *, const Cmd *>> *calls) {
  PropDB db(fn);
  const auto &entry_block = fn->blocks_[0];
  auto &view              = db.views_[&entry_block];
  for (i32 i = 0; i < static_cast<i32>(args.size()); ++i) {
    view.props_[Register(i)] = std::move(args[i]);
    db.MarkReferencesStale(entry_block,
                           CmdIndex{0, i - static_cast<i32>(args.size())});
  }

  for (i32 i = 0; i < static_cast<i32>(fn->blocks_.size()); ++i) {
    const auto& block = fn->blocks_[i];
    for (i32 j = 0; j < static_cast<i32>(block.cmds_.size()); ++j) {
      const auto &cmd = block.cmds_[j];
      // It's possible that we don't look at the arguments at all. To be safe we
      // need to start with everything as stale.

      db.stale_.emplace(&fn->block(fn->entry()), CmdIndex{BlockIndex{i}, j});

      if (cmd.op_code_ == Op::Call) {
        if (auto *func = std::get_if<Func *>(&cmd.args.back().value)) {
          validation_queue->push(*func);
          if (!(*func)->preconditions_.empty()) {
            // TODO only if it has precondiitons
            calls->emplace_back(&block, &cmd);
          }
        }
      }
    }
  }

  return db;
}
} // namespace property

// Roughly the same as executing the function but now with more generic
// argument properties.
static bool
ValidateRequirement(const Func *fn,
                    std::vector<base::owned_ptr<property::Property>> args,
                    std::queue<Func *> *validation_queue) {
  // TODO. In this case we don't care about the calls vector at all. Ideally, we
  // should template it away.
  std::vector<std::pair<const Block *, const Cmd *>> calls;
  auto prop_db =
      property::PropDB::Make(fn, std::move(args), validation_queue, &calls);
  prop_db.Compute();
  // TODO BlockIndex(1) may not be the only return!
  for (BlockIndex return_block : fn->return_blocks_) {
    auto *bool_prop = ptr_cast<property::BoolProperty>(
        prop_db.views_[&fn->block(return_block)]
            .ret_props_[ReturnValue(0)]
            .get());
    if (bool_prop->kind != property::BoolProperty::Kind::True) { return false; }
  }
  return true;
}

int Func::ValidateCalls(std::queue<Func *> *validation_queue) const {
  if (num_errors_ >= 0) { return num_errors_; }
  num_errors_ = 0;
  if (debug::no_validation) { return num_errors_; }

  std::vector<std::pair<const Block *, const Cmd *>> calls;
  auto prop_db = property::PropDB::Make(this, {}, validation_queue, &calls);

  if (calls.empty() && postconditions_.empty()) {
    // TODO This can be determined before even creating the property map.
    return num_errors_;
  }

  prop_db.Compute();

  for (const auto & [ calling_block, cmd ] : calls) {
    Func *called_fn = std::get<Func *>(cmd->args.back().value);
    // 'args' Also includes the function as the very last entry.
    std::vector<base::owned_ptr<property::Property>> arg_props;
    arg_props.reserve(cmd->args.size() - 1);
    for (const auto& arg : cmd->args) {
      arg_props.push_back(prop_db.Get(*calling_block, arg));
    }
    for (const auto &precondition : called_fn->preconditions_) {
      auto ir_fn =
          ExprFn(precondition, called_fn->type->input, IR::Cmd::Kind::Exec);
      if (!ValidateRequirement(ir_fn.get(), arg_props, validation_queue)) {
        LOG << "Failed a precondition.";
        // TODO log error
        ++num_errors_;
      }
    }
  }

  for (const auto &postcondition : postconditions_) {
    std::vector<Type *> input_types = type->input->is<Tuple>()
                                          ? type->input->as<Tuple>().entries
                                          : std::vector<Type *>{type->input};
    i32 num_outs;
    if (type->output->is<Tuple>()) {
      auto out_tup = type->output->as<Tuple>();
      input_types.insert(input_types.end(), out_tup.entries.begin(),
                         out_tup.entries.end());
      num_outs = static_cast<i32>(out_tup.entries.size());
    } else {
      input_types.push_back(type->output);
      num_outs = 1;
    }

    std::vector<base::owned_ptr<property::Property>> arg_props;
    arg_props.reserve(input_types.size());
    // TODO multiple exit blocks

    if (return_blocks_.size() != 1) {
      NOT_YET("Not a real assertion, just not handling this case yet.");
    }

    const Block *exit_block = &block(*return_blocks_.begin());
    for (i32 i = 0; i < static_cast<i32>(args_.size()); ++i) {
      arg_props.push_back(prop_db.views_[exit_block].props_[Register(i)]);
    }
    for (i32 i = 0; i < num_outs; ++i) {
      arg_props.push_back(
          prop_db.views_[exit_block].ret_props_[ReturnValue(i)]);
    }

    auto ir_fn =
        ExprFn(postcondition, Tup(input_types), IR::Cmd::Kind::PostCondition);
    if (!ValidateRequirement(ir_fn.get(), arg_props, validation_queue)) {
      LOG << "Failed post condition";
    }
  }

  return num_errors_;
}
} // namespace IR
