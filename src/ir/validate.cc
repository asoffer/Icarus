#include "ir.h"

#include "../error_log.h"
#include "property.h"

namespace IR {
namespace property {
struct Ordering {
  bool operator()(const std::pair<const Block *, Register> &lhs,
                  const std::pair<const Block *, Register> &rhs) const {
    if (lhs.first < rhs.first) { return true; }
    if (lhs.first > rhs.first) { return false; }
    return lhs.second.value < rhs.second.value;
  }
};

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
  void MarkReferencesStale(const Block &block, Register reg);

  Range<i32> GetIntProperty(const Block &block, Val arg);
  void Compute();

  const Func *fn_;
  // TODO allow multiple properties
  std::unordered_map<const Block *,
                     std::unordered_map<Register, std::unique_ptr<Property>>>
      properties_;
  std::vector<std::unique_ptr<Property>> return_properties_;

  // TODO this should be unordered, but I haven't written a hash yet.
  std::set<std::pair<const Block *, Register>, Ordering> stale_;

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
  auto &block_entry = properties_[&block];
  auto iter         = block_entry.find(reg);
  if (iter->second.get() == nullptr) {
    return nullptr;
  } else if (iter != block_entry.end()) {
    return base::own(iter->second->Clone());
  } else {
    ASSERT_EQ(block.incoming_blocks_.size(), 1);
    return GetProp(fn_->block(block.incoming_blocks_[0]), reg);
  }
}

void PropertyMap::SetProp(const Block &block, const Cmd &cmd,
                          base::owned_ptr<Property> prop) {
  if (cmd.type == nullptr || cmd.type == Void) { return; }
  auto &block_entry       = properties_[&block];
  block_entry[cmd.result] = std::move(prop);

  MarkReferencesStale(block, cmd.result);
}

void PropertyMap::MarkReferencesStale(const Block &block, Register reg) {
  auto iter = fn_->references_.find(reg);
  for (const auto &cmd_index : iter->second) {
    const auto &stale_cmd = fn_->Command(cmd_index);
    if (stale_cmd.type == nullptr || stale_cmd.type == Void) { continue; }
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

Range<i32> PropertyMap::GetIntProperty(const Block &block, Val arg) {
  if (arg.value.is<Register>()) {
    auto reg = arg.value.as<Register>();
    return *ptr_cast<Range<i32>>(GetProp(block, reg).get());
  } else if (arg.value.is<i32>()) {
    return Range<i32>(arg.value.as<i32>(), arg.value.as<i32>());
  }
  UNREACHABLE();
}

void PropertyMap::Compute() {
  while (!stale_.empty()) {
    auto iter          = stale_.begin();
    const Block *block = iter->first;
    const Register reg = iter->second;
    // TODO think about arguments being marked as stale
    if (reg.value < static_cast<i32>(fn_->num_args)) {
      LOG << "argument" << reg << " was marked as stale.";
      stale_.erase(iter);
      continue;
    }

    const Cmd &cmd     = fn_->Command(reg);
    auto prop          = GetProp(*block, cmd.result);

    stale_.erase(iter);

    switch (cmd.op_code) {
    case Op::Add: {
      if (cmd.type == Int) {
        auto new_prop = GetIntProperty(*block, cmd.args[0]) +
                        GetIntProperty(*block, cmd.args[1]);
        if (!prop->Implies(new_prop)) {
          // TODO move instead of copy here?
          SetProp(*block, cmd, base::own(new_prop.Clone()));
        }
      } else {
        NOT_YET();
      }
    } break;
    case Op::Call:
      // TODO No post-conditions yet, so nothing to see here.
      continue;
    case Op::Lt: {
      if (cmd.args[0].type == Int) {
        ASSERT(prop.get() == nullptr, "Why else was it stale?");
        auto new_prop = (GetIntProperty(*block, cmd.args[0]) <
                         GetIntProperty(*block, cmd.args[1]));
        SetProp(*block, cmd, base::own(new_prop->Clone()));
      } else {
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
  auto &start_block_data = prop_map.properties_[&fn->block(fn->entry())];
  for (i32 i = 0; i < static_cast<i32>(args.size()); ++i) {
    start_block_data[Register(i)] = args[i];
  }

  // Initialize everything as stale.
  for (const auto &block : fn->blocks_) {
    auto &block_data = prop_map.properties_[&block];
    for (const auto &cmd : block.cmds_) {
      if (cmd.type == nullptr || cmd.type == Void) { continue; }
      if (cmd.type == Int) {
        auto iter =
            block_data
                .emplace(cmd.result, std::make_unique<Range<i32>>())
                .first;
        prop_map.stale_.emplace(&block, iter->first);
      } else if (cmd.type == Bool) {
        auto iter = block_data.emplace(cmd.result, nullptr).first;
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
                     std::vector<base::owned_ptr<IR::property::Property>> args,
                     std::queue<Func *> *validation_queue) {
  // TODO. In this case we don't care about the calls vector at all. Ideally, we
  // should template it away.
  std::vector<std::pair<const Block *, const Cmd *>> calls;
  auto prop_map = property::PropertyMap::Make(fn, std::move(args),
                                              validation_queue, &calls);
  prop_map.Compute();

  // TODO verify that the return value has the property "true"
  return true;
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
      } else {
        NOT_YET();
      }
    }
    Type *input_type = called_fn->type->as<Function>().input;
    for (const auto &precondition : called_fn->preconditions_) {
      auto ir_fn = ExprFn(precondition, input_type);
      if (!ValidatePrecondition(ir_fn.get(), arg_props, validation_queue)) {
        // TODO log error
        ++num_errors_;
      }
    }
  }

  // TODO implement me
  return num_errors_;
}
} // namespace IR
