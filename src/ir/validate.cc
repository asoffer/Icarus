#include "ir.h"

#include "../error_log.h"
#include "property.h"

namespace IR {
namespace property {
struct PropView {
  std::unordered_map<Register, base::owned_ptr<Property>> props_;
  std::unordered_map<ReturnValue, base::owned_ptr<Property>> ret_props_;
  std::unordered_map<const Block *, std::unordered_set<const Block *>>
      incoming_;
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
      switch (last_cmd.op_code) {
      case Op::UncondJump:
        incoming[&fn_->block(last_cmd.args[0].value.as<BlockIndex>())].insert(
            &block);
        break;
      case Op::CondJump:
        incoming[&fn_->block(last_cmd.args[1].value.as<BlockIndex>())].insert(
            &block);
        incoming[&fn_->block(last_cmd.args[2].value.as<BlockIndex>())].insert(
            &block);
        break;
      case Op::ReturnJump: /* Nothing to do */ break;
      default: UNREACHABLE();
      }
    }

    for (const auto &viewing_block : fn_->blocks_) {
      auto &view = views_[&viewing_block];
      if (fn_->num_args_ == 1) {
        auto prop = DefaultProperty(fn_->type->input);
        ASSERT(prop.get() != nullptr, "");
        view.props_[Register(0)] = std::move(prop);
      } else if (fn_->num_args_ > 1) {
        for (i32 i = 0; i < static_cast<i32>(fn_->num_args_); ++i) {
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

      // TODO store num_rets. (on function type), or have it computable.
      i32 num_rets;
      if (fn_->type->output->is<Tuple>()) {
        num_rets =
            static_cast<i32>(fn_->type->output->as<Tuple>().entries.size());
      } else if (fn_->type->output == Void) {
        num_rets = 0;
      } else {
        num_rets = 1;
      }
      // TODO should be stored on PropView to get per-block views of return
      // values.
      for (i32 i = 0; i < num_rets; ++i) {
        view.ret_props_[ReturnValue(i)] = base::make_owned<BoolProperty>();
      }
    }

    // Only mark things on the entry view and entry block as stale to begin
    // with. All else can be propogated.
    const auto& entry_block = fn_->blocks_[0];
    for (const auto &cmd : entry_block.cmds_) {
      if (cmd.type == nullptr || cmd.type == Void) { continue; }
      MarkReferencesStale(entry_block, cmd.result);
    }
  }

  PropDB(PropDB &&) = default;

  void MarkReferencesStale(const Block &block, Register reg) {
    auto iter = fn_->references_.find(reg);
    ASSERT(iter != fn_->references_.end(), "");
    const auto &references = iter->second;

    for (const auto &cmd_index : references) {
      const auto &stale_cmd = fn_->Command(cmd_index);
      stale_.emplace(&block, stale_cmd.result);
    }

    const auto &last_cmd = block.cmds_.back();
    switch (last_cmd.op_code) {
    case Op::UncondJump:
      stale_.emplace(&fn_->block(last_cmd.args[0].value.as<BlockIndex>()), reg);
      break;
    case Op::CondJump:
      stale_.emplace(&fn_->block(last_cmd.args[1].value.as<BlockIndex>()), reg);
      stale_.emplace(&fn_->block(last_cmd.args[2].value.as<BlockIndex>()), reg);
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

  void Set(const Block &block, Register reg,
           base::owned_ptr<Property> new_prop) {
    auto &old_prop = views_[&block].props_[reg];
    if (!old_prop->Implies(*new_prop)) {
      block.dump(28);
      std::cerr << "\n";
      fn_->Command(reg).dump(28);
      LOG << "=> " << *new_prop;
      std::cerr << "\n";

      old_prop = std::move(new_prop);
      MarkReferencesStale(block, reg);
    }
  }

  void Set(const Block &block, ReturnValue ret,
           base::owned_ptr<Property> new_prop) {
    // TODO merge these instead of just assigning
    auto &old_prop = views_[&block].ret_props_[ret];
    if (!old_prop->Implies(*new_prop)) {
      LOG << "In view " << &block << " replacing " << *old_prop << " with "
          << *new_prop;
      old_prop = std::move(new_prop);
    }
  }

  void SetUnreachable(const Block &view, const Block &from, const Block &to) {
    LOG << &from << " => " << &to << " Is now unreachable.";
    // TODO clean up stales still referencing this blocks.
    auto &reach_set = views_[&view].incoming_[&to];
    reach_set.erase(&from);
    if (!reach_set.empty()) { return; }

    // TODO What about weak back-references? We don't yet handle loops.
    const auto &last_cmd = to.cmds_.back();
    switch (last_cmd.op_code) {
    case Op::UncondJump:
      SetUnreachable(view, to,
                     fn_->block(last_cmd.args[0].value.as<BlockIndex>()));
      break;
    case Op::CondJump:
      SetUnreachable(view, to,
                     fn_->block(last_cmd.args[1].value.as<BlockIndex>()));
      SetUnreachable(view, to,
                     fn_->block(last_cmd.args[2].value.as<BlockIndex>()));

      break;
    case Op::ReturnJump: /* Nothing to do */ break;
    default: UNREACHABLE();
    }
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

  base::owned_ptr<Range<i32>> GetIntProp(const Block &block, Val arg) {
    if (arg.value.is<Register>()) {
      auto reg = arg.value.as<Register>();
      return Get(block, reg).as<Range<i32>>();
    } else if (arg.value.is<i32>()) {
      return base::make_owned<Range<i32>>(arg.value.as<i32>(),
                                          arg.value.as<i32>());
    }
    UNREACHABLE();
  }

  base::owned_ptr<BoolProperty> GetBoolProp(const Block &block, Val arg) {
    if (arg.value.is<Register>()) {
      auto reg = arg.value.as<Register>();
      return Get(block, reg).as<BoolProperty>();
    } else if (arg.value.is<bool>()) {
      return base::make_owned<BoolProperty>(arg.value.as<bool>());
    }
    UNREACHABLE();
  }

  void Compute();

  const Func *fn_;
  std::unordered_map<const Block *, PropView> views_;
  // TODO should be an unordered_set but I haven't define an appropriate hash
  // function for the pair yet.
  std::set<std::pair<const Block *, Register>> stale_;
  // TODO should be per block
};

void PropDB::Compute() {
  while (!stale_.empty()) {
    auto iter          = stale_.begin();
    const Block *block = iter->first;
    const Register reg = iter->second;
    if (reg.is_arg(*fn_)) {
      // TODO arguments can be stale: viewed from a different block you might
      // condition on them having some property and want to use that in
      // verification. You need to handle this properly which likely just
      // entails merging from incoming blocks.
      stale_.erase(iter);
      continue;
    }

    const Cmd &cmd = fn_->Command(reg);
    auto prop      = Get(*block, cmd.result);
    stale_.erase(iter);

    switch (cmd.op_code) {
    case Op::Neg: {
      if (cmd.type == Int) {
        Set(*block, cmd.result, -GetIntProp(*block, cmd.args[0]));
      } else if (cmd.type == Bool) {
        Set(*block, cmd.result, !GetBoolProp(*block, cmd.args[0]));
      } else {
        UNREACHABLE();
      }
    } break;
#define CASE(case_name, case_sym)                                              \
  case Op::case_name: {                                                        \
    if (cmd.type == Int) {                                                     \
      Set(*block, cmd.result, GetIntProp(*block, cmd.args[0])                  \
                                  case_sym GetIntProp(*block, cmd.args[1]));   \
    } else {                                                                   \
      NOT_YET();                                                               \
    }                                                                          \
  } break
      CASE(Add, +);
      CASE(Sub, -);
      CASE(Mul, *);
#undef CASE
#define CASE(case_name, case_sym)                                              \
  case Op::case_name: {                                                        \
    if (cmd.args[0].type == Int) {                                             \
      Set(*block, cmd.result, GetIntProp(*block, cmd.args[0])                  \
                                  case_sym GetIntProp(*block, cmd.args[1]));   \
    } else {                                                                   \
      NOT_YET();                                                               \
    }                                                                          \
  } break
      CASE(Lt, <);
      CASE(Le, <=);
      CASE(Ge, >=);
      CASE(Gt, >);
#undef CASE
    case Op::Xor: {
      Set(*block, cmd.result,
          GetBoolProp(*block, cmd.args[0]) ^ GetBoolProp(*block, cmd.args[1]));

    } break;
    case Op::Phi: {
      if (cmd.type == Bool) {
        std::vector<BoolProperty> props;
        for (size_t i = 0; i < cmd.args.size(); i += 2) {
          if (Reachable(
                  /* view = */ *block,
                  /* from = */ fn_->block(cmd.args[i].value.as<BlockIndex>()),
                  /*   to = */ fn_->block(reg))) {
            props.push_back(*GetBoolProp(*block, cmd.args[i + 1]));
          }
        }

        if (props.empty()) {
          // This block is no longer reachable!
          LOG << "No longer reachable";
        } else {
          // TODO this is not a great way to do merging because it's not genric,
          // but it's simple for now.
          Set(*block, cmd.result, BoolProperty::Merge(props));
        }
      } else {
        NOT_YET();
      }
    } break;
    case Op::Print: break;
    case Op::Call:
      // TODO No post-conditions yet, so nothing to see here.
      continue;
    case Op::Nop: break;
    case Op::SetReturn: {
      if (cmd.args[1].value.is<Register>()) {
        Set(*block, cmd.args[0].value.as<ReturnValue>(),
            Get(*block, cmd.args[1].value.as<Register>()));
      } else {
        LOG << "Non-register return " << cmd.args[1].to_string();
        NOT_YET();
      }
    } break;
    case Op::CondJump: {
      auto prop       = Get(*block, cmd.args[0].value.as<Register>());
      auto *bool_prop = dynamic_cast<BoolProperty *>(prop.get());
      if (bool_prop == nullptr) { continue; }
      auto &true_block  = fn_->block(cmd.args[1].value.as<BlockIndex>());
      auto &false_block = fn_->block(cmd.args[2].value.as<BlockIndex>());

      if (bool_prop->kind == BoolProperty::Kind::True) {
        SetUnreachable(/* view = */ *block, /* from = */ fn_->block(reg),
                       /*   to = */ false_block);
      } else if (bool_prop->kind == BoolProperty::Kind::False) {
        SetUnreachable(/* view = */ *block, /* from = */ fn_->block(reg),
                       /*   to = */ true_block);
      }
    } break;
    default:
      LOG << "Not yet handled: " << static_cast<int>(cmd.op_code);
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
    db.MarkReferencesStale(entry_block, Register(i));
  }

  for (const auto &block : fn->blocks_) {
    for (const auto &cmd : block.cmds_) {
      if (cmd.op_code == Op::Call) {
        const auto &called_fn = cmd.args.back().value;
        if (called_fn.is<Func *>()) {
          validation_queue->push(called_fn.as<Func *>());
          if (!called_fn.as<Func *>()->preconditions_.empty()) {
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
ValidatePrecondition(const Func *fn,
                     std::vector<base::owned_ptr<property::Property>> args,
                     std::queue<Func *> *validation_queue) {
  // TODO. In this case we don't care about the calls vector at all. Ideally, we
  // should template it away.
  std::vector<std::pair<const Block *, const Cmd *>> calls;
  auto prop_db =
      property::PropDB::Make(fn, std::move(args), validation_queue, &calls);
  prop_db.Compute();
  // TODO BlockIndex(1) may not be the only return!
  auto *bool_prop =
      ptr_cast<property::BoolProperty>(prop_db.views_[&fn->block(BlockIndex(1))]
                                           .ret_props_[ReturnValue(0)]
                                           .get());
  return bool_prop->kind == property::BoolProperty::Kind::True;
}

int Func::ValidateCalls(std::queue<Func *> *validation_queue) const {
  if (num_errors_ >= 0) { return num_errors_; }
  num_errors_ = 0;

  std::vector<std::pair<const Block *, const Cmd *>> calls;
  auto prop_db = property::PropDB::Make(this, {}, validation_queue, &calls);

  if (calls.empty()) {
    // TODO This can be determined before even creating the property map.
    return num_errors_;
  }
  prop_db.Compute();

  for (const auto &call : calls) {
    Func *called_fn            = call.second->args.back().value.as<Func *>();
    const Block &calling_block = *call.first;

    // 'args' Also includes the function as the very last entry.
    const auto &args = call.second->args;
    std::vector<base::owned_ptr<property::Property>> arg_props;
    arg_props.reserve(args.size() - 1);
    for (size_t i = 0; i < args.size() - 1; ++i) {
      const auto &argument = args[i].value;
      if (argument.is<Register>()) {
        arg_props.push_back(
            prop_db.Get(calling_block, argument.as<Register>()));
      } else if (argument.is<i32>()) {
        arg_props.push_back(base::make_owned<property::Range<i32>>(
            argument.as<i32>(), argument.as<i32>()));
      } else if (argument.is<bool>()) {
        arg_props.push_back(
            base::make_owned<property::BoolProperty>(argument.as<bool>()));
      } else {
        LOG << args[i].to_string();
        NOT_YET();
      }
    }
    Type *input_type = called_fn->type->as<Function>().input;
    for (const auto &precondition : called_fn->preconditions_) {
      auto ir_fn = ExprFn(precondition, input_type);
      if (!ValidatePrecondition(ir_fn.get(), arg_props, validation_queue)) {
        LOG << "Failed a precondition.";
        // TODO log error
        ++num_errors_;
      }
    }
  }

  // TODO implement me
  return num_errors_;
}
} // namespace IR
