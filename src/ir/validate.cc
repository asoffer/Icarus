#include "ir.h"

#include "../error_log.h"
#include "property.h"

namespace IR {
namespace property {
struct PropertyMap {
public:
  PropertyMap(PropertyMap &&) = default;

  static PropertyMap
  Make(const Func *fn, std::vector<base::owned_ptr<property::Property>> args,
       std::queue<Func *> *validation_queue,
       std::vector<std::pair<const Block *, const Cmd *>> *calls);

  base::owned_ptr<Property> GetProp(const Block &block, Register reg);
  void SetProp(const Block &block, const Cmd &cmd,
               base::owned_ptr<Property> prop);
  void SetProp(ReturnValue ret, base::owned_ptr<Property> prop);
  void MarkReferencesStale(const Block &block, Register reg);

  base::owned_ptr<Range<i32>> GetIntProperty(const Block &block, Val arg);
  base::owned_ptr<BoolProperty> GetBoolProperty(const Block &block, Val arg);
  void Compute();

  const Func *fn_;
  // TODO allow multiple properties
  std::unordered_map<const Block *,
                     std::unordered_map<Register, base::owned_ptr<Property>>>
      properties_;

  // Probably better to use a vector, but for type safety, using a map.
  // TODO: Type safe vectors.
  std::unordered_map<ReturnValue, base::owned_ptr<Property>> return_properties_;

  // TODO this should be unordered, but I haven't written a hash yet.
  std::set<std::pair<const Block *, Register>> stale_;


private:
  PropertyMap()                    = delete;
  PropertyMap(const PropertyMap &) = delete;
  explicit PropertyMap(const Func *fn)
      : fn_(fn),
        return_properties_(fn->type->output->is<Tuple>()
                               ? fn->type->output->as<Tuple>().entries.size()
                               : 1) {}
};

base::owned_ptr<Property> PropertyMap::GetProp(const Block &block,
                                               Register reg) {
  return properties_[&block].find(reg)->second;
}

void PropertyMap::SetProp(const Block &block, const Cmd &cmd,
                          base::owned_ptr<Property> new_prop) {
  if (cmd.type == nullptr || cmd.type == Void) { return; }
  auto &old_prop = properties_[&block][cmd.result];
  if (!old_prop->Implies(*new_prop)) {
    old_prop = std::move(new_prop);
    MarkReferencesStale(block, cmd.result);
  }
}

void PropertyMap::SetProp(ReturnValue ret, base::owned_ptr<Property> prop) {
  // TODO merge these instead of just assigning
  return_properties_[ret] = std::move(prop);
}

void PropertyMap::MarkReferencesStale(const Block &block, Register reg) {
  auto iter = fn_->references_.find(reg);

  for (const auto &cmd_index : iter->second) {
    const auto &stale_cmd = fn_->Command(cmd_index);
    stale_.emplace(&block, stale_cmd.result);
    switch (block.jmp_.type) {
    case Jump::Type::Uncond:
      stale_.emplace(&fn_->block(block.jmp_.block_index), stale_cmd.result);
      break;
    case Jump::Type::Cond:
      stale_.emplace(&fn_->block(block.jmp_.cond_data.true_block),
                     stale_cmd.result);
      stale_.emplace(&fn_->block(block.jmp_.cond_data.false_block),
                     stale_cmd.result);
      break;
    case Jump::Type::Ret: /* Nothing to do */ break;
    case Jump::Type::None: UNREACHABLE();
    }
  }
}

base::owned_ptr<Range<i32>> PropertyMap::GetIntProperty(const Block &block,
                                                        Val arg) {
  if (arg.value.is<Register>()) {
    auto reg = arg.value.as<Register>();
    return GetProp(block, reg).as<Range<i32>>();
  } else if (arg.value.is<i32>()) {
    return base::make_owned<Range<i32>>(arg.value.as<i32>(),
                                        arg.value.as<i32>());
  }
  UNREACHABLE();
}

base::owned_ptr<BoolProperty> PropertyMap::GetBoolProperty(const Block &block,
                                                           Val arg) {
  if (arg.value.is<Register>()) {
    auto reg = arg.value.as<Register>();
    return GetProp(block, reg).as<BoolProperty>();
  } else if (arg.value.is<bool>()) {
    return base::make_owned<BoolProperty>(arg.value.as<bool>());
  }
  UNREACHABLE();
}

void PropertyMap::Compute() {
  while (!stale_.empty()) {
    auto iter          = stale_.begin();
    const Block *block = iter->first;
    const Register reg = iter->second;
    // TODO think about arguments being marked as stale
    if (reg.is_arg(*fn_)) {
      LOG << "argument" << reg << " was marked as stale.";
      stale_.erase(iter);
      continue;
    }

    const Cmd &cmd     = fn_->Command(reg);
    auto prop          = GetProp(*block, cmd.result);
    stale_.erase(iter);

    switch (cmd.op_code) {
    case Op::Neg: {
      if (cmd.type == Int) {
        SetProp(*block, cmd, -GetIntProperty(*block, cmd.args[0]));
      } else if (cmd.type == Bool) {
        SetProp(*block, cmd, !GetBoolProperty(*block, cmd.args[0]));
      } else {
        UNREACHABLE();
      }
    } break;
#define CASE(case_name, case_sym)                                              \
  case Op::case_name: {                                                        \
    if (cmd.type == Int) {                                                     \
      SetProp(*block, cmd, GetIntProperty(*block, cmd.args[0])                 \
                               case_sym GetIntProperty(*block, cmd.args[1]));  \
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
      SetProp(*block, cmd, GetIntProperty(*block, cmd.args[0])                 \
                               case_sym GetIntProperty(*block, cmd.args[1]));  \
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
      SetProp(*block, cmd, GetBoolProperty(*block, cmd.args[0]) ^
                               GetBoolProperty(*block, cmd.args[1]));

    } break;
    case Op::Phi: {
      if (cmd.type == Bool) {
        std::vector<BoolProperty> props;
        fn_->dump();
        for (size_t i = 1; i < cmd.args.size(); i += 2) {
          props.push_back(*GetBoolProperty(*block, cmd.args[i]));
          LOG << props.back();
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
        SetProp(cmd.args[0].value.as<ReturnValue>(),
                GetProp(*block, cmd.args[1].value.as<Register>()));
      } else {
        LOG << "Non-register return " << cmd.args[1].to_string();
        NOT_YET();
      }
    } break;
    default:
      LOG << "Not yet handled: " << static_cast<int>(cmd.op_code);
      continue;
    }
  }
}

PropertyMap
PropertyMap::Make(const Func *fn, std::vector<base::owned_ptr<Property>> args,
                  std::queue<Func *> *validation_queue,
                  std::vector<std::pair<const Block *, const Cmd *>> *calls) {
  PropertyMap prop_map(fn);

  for (const auto &block : fn->blocks_) {
    auto &block_data = prop_map.properties_[&block];
    for (i32 i = 0; i < static_cast<i32>(args.size()); ++i) {
      block_data[Register(i)] = args[i];
    }
  }

  // Initialize everything as stale.
  for (const auto &block : fn->blocks_) {
    auto &block_data = prop_map.properties_[&block];
    for (const auto &cmd : block.cmds_) {
      if (cmd.type == nullptr || cmd.type == Void) {
        switch (cmd.op_code) {
        case Op::Print: {
        } break;
        case Op::SetReturn: {
          auto iter = block_data.emplace(cmd.result, nullptr).first;
          prop_map.stale_.emplace(&block, iter->first);
        } break;
        default: {
          cmd.dump(10);
          LOG << "Not yet handled.";
        }
        }
      } else if (cmd.type == Int) {
        auto iter =
            block_data.emplace(cmd.result, base::make_owned<Range<i32>>())
                .first;
        prop_map.stale_.emplace(&block, iter->first);
      } else if (cmd.type == Bool) {
        auto iter =
            block_data
                .emplace(cmd.result, base::make_owned<property::BoolProperty>())
                .first;
        prop_map.stale_.emplace(&block, iter->first);
      }

      // TODO actually, only put call in place if it needs to be verified
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
  return prop_map;
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
  auto prop_map = property::PropertyMap::Make(fn, std::move(args),
                                              validation_queue, &calls);
  prop_map.Compute();
  auto *bool_prop = ptr_cast<property::BoolProperty>(
      prop_map.return_properties_[ReturnValue(0)].get());
  return bool_prop->kind == property::BoolProperty::Kind::True;
}

int Func::ValidateCalls(std::queue<Func *> *validation_queue) const {
  if (num_errors_ >= 0) { return num_errors_; }
  num_errors_ = 0;

  std::vector<std::pair<const Block *, const Cmd *>> calls;
  auto prop_map =
      property::PropertyMap::Make(this, {}, validation_queue, &calls);

  if (calls.empty()) {
    // TODO This can be determined before even creating the property map.
    return num_errors_;
  }
  prop_map.Compute();

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
            prop_map.GetProp(calling_block, argument.as<Register>()));
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
