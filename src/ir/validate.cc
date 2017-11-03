#include "ir.h"

#include "../error_log.h"
#include "property.h"

std::unique_ptr<IR::Func> ExprFn(AST::Expression *expr);

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
  PropertyMap()                    = delete;
  PropertyMap(const PropertyMap &) = delete;
  PropertyMap(PropertyMap &&)      = default;
  explicit PropertyMap(const Func *fn) : fn_(fn) {}

  const Func *fn_;
  Property *Get(const Block &block, const Cmd &cmd);

  // TODO allow multiple properties
  std::unordered_map<const Block *,
                     std::unordered_map<Register, std::unique_ptr<Property>>>
      properties_;
  // TODO this should be unordered, but I haven't written a hash yet.
  std::set<std::pair<const Block *, Register>, Ordering> stale_;
};

Property *PropertyMap::Get(const Block &block, const Cmd &cmd) {
  if (cmd.type == nullptr || cmd.type == Void) { return nullptr; }
  auto &block_entry = properties_[&block];
  auto iter         = block_entry.find(cmd.result);
  if (iter != block_entry.end()) { return iter->second.get(); }
  NOT_YET();
}
} // namespace property

static property::PropertyMap
MakePropertyMap(const Func *fn, std::vector<property::Property *> args,
                std::queue<Func *> *validation_queue,
                std::vector<std::pair<const Block *, const Cmd *>> *calls) {
  property::PropertyMap prop_map(fn);
  auto &start_block_data = prop_map.properties_[&fn->block(fn->entry())];
  for (i32 i = 0; i < static_cast<i32>(args.size()); ++i) {
    (void)start_block_data;
  }

  for (const auto &block : fn->blocks_) {
    auto &block_data = prop_map.properties_[&block];
    for (const auto &cmd : block.cmds_) {
      if (cmd.type == nullptr || cmd.type == Void) { continue; }
      if (cmd.type == Int) {
        auto iter =
            block_data
                .emplace(cmd.result, std::make_unique<property::Range<i32>>())
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

// Roughly the same as executing the function but now with more generic
// argument properties.
static bool
ValidatePrecondition(const Func *fn,
                     const std::vector<IR::property::Property *> &args,
                     std::queue<Func *> *validation_queue) {
  // TODO. In this case we don't care about the calls vector at all. Ideally, we
  // should template it away.
  std::vector<std::pair<const Block *, const Cmd *>> calls;
  auto prop_map = MakePropertyMap(fn, args, validation_queue, &calls);

  // TODO verify that the return value has the property "true"
  return true;
}

int Func::ValidateCalls(std::queue<Func *> *validation_queue) const {
  if (num_errors_ >= 0) { return num_errors_; }
  num_errors_ = 0;

  std::vector<std::pair<const Block *, const Cmd *>> calls;
  auto prop_map = MakePropertyMap(this, {}, validation_queue, &calls);

  if (calls.empty()) { return num_errors_; }

  while (!prop_map.stale_.empty()) {
    auto iter = prop_map.stale_.begin();
    prop_map.stale_.erase(iter);
    const Block *block       = iter->first;
    const Register reg       = iter->second;
    const Cmd &cmd           = Command(reg);
    property::Property *prop = prop_map.Get(*block, cmd);
    if (prop == nullptr) { continue; }

    switch (cmd.op_code) {
    case Op::Add: {
      property::Range<i32> arg0, arg1;
      if (cmd.type == Int) {
        if (cmd.args[0].value.is<Register>()) {
          // TODO handling arguments should maybe be done elsewhere.
          auto arg0_reg = cmd.args[0].value.as<Register>();
          if (static_cast<size_t>(arg0_reg.value) >= num_args) {
            arg0 = *ptr_cast<property::Range<i32>>(
                prop_map.Get(*block, Command(arg0_reg)));
          }
        } else if (cmd.args[0].value.is<i32>()) {
          arg0 = property::Range<i32>(cmd.args[0].value.as<i32>(),
                                      cmd.args[0].value.as<i32>());
        } else {
          UNREACHABLE();
        }

        if (cmd.args[1].value.is<Register>()) {
          // TODO handling arguments should maybe be done elsewhere.
          auto arg1_reg   = cmd.args[1].value.as<Register>();
          size_t num_args = 1;
          if (type->input->is<Tuple>()) {
            num_args = type->input->as<Tuple>().entries.size();
          }
          if (static_cast<size_t>(arg1_reg.value) >= num_args) {
            arg1 = *ptr_cast<property::Range<i32>>(
                prop_map.Get(*block, Command(arg1_reg)));
          }
        } else if (cmd.args[1].value.is<i32>()) {
          arg1 = property::Range<i32>(cmd.args[1].value.as<i32>(),
                                      cmd.args[1].value.as<i32>());
        } else {
          UNREACHABLE();
        }

        auto new_prop = arg0 + arg1;
        if (!prop->Implies(&new_prop)) {
          *prop     = std::move(new_prop);
          auto iter = references_.find(cmd.result);
          for (const auto &cmd_index : iter->second) {
            auto *stale_cmd = &Command(cmd_index);
            prop_map.stale_.emplace(block, stale_cmd->result);
            switch (block->jmp_.type) {
            case Jump::Type::Uncond:
              prop_map.stale_.emplace(&this->block(block->jmp_.block_index),
                                      stale_cmd->result);
              break;
            case Jump::Type::Cond:
              prop_map.stale_.emplace(
                  &this->block(block->jmp_.cond_data.true_block),
                  stale_cmd->result);
              prop_map.stale_.emplace(
                  &this->block(block->jmp_.cond_data.false_block),
                  stale_cmd->result);
              break;
            default: NOT_YET();
            }
          }
        }
      } else {
        NOT_YET();
      }
    } break;
    default: LOG << "Not yet handled"; continue;
    }
  }

  for (const auto &call : calls) {
    Func *called_fn            = call.second->args.back().value.as<Func *>();
    const Block &calling_block = *call.first;

    // Some properties already exist, such as those for registers. Others need
    // to be created such as for constants. Those already existing are already
    // owned in a unique_ptr, so we just want to have raw pointers to them.
    // However, this is dangerous for those we want to create on the fly right
    // now. The solution is to have a temporary vector of unique_ptrs to
    // properties and then pull out the raw pointers we desire.
    std::vector<std::unique_ptr<property::Property>> temps;

    // 'args' Also includes the function as the very last entry.
    const auto &args = call.second->args;
    std::vector<property::Property *> arg_props;
    arg_props.reserve(args.size() - 1);
    for (size_t i = 0; i < args.size() - 1; ++i) {
      const auto &argument = args[i].value;
      if (argument.is<Register>()) {
        arg_props.push_back(
            prop_map.Get(calling_block, Command(argument.as<Register>())));
      } else if (argument.is<i32>()) {
        temps.push_back(std::make_unique<property::Range<i32>>(
            argument.as<i32>(), argument.as<i32>()));
        arg_props.push_back(temps.back().get());
      } else {
        NOT_YET();
      }
    }

    for (const auto &precondition : called_fn->preconditions_) {
      auto ir_fn = ExprFn(precondition);
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
