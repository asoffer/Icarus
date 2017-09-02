#include "ir.h"

#include "../error_log.h"

namespace IR {
int Func::ValidateCalls(std::queue<IR::Func *> *validation_queue) {
  if (num_errors_ >= 0) { return num_errors_; }
  num_errors_ = 0;

  std::queue<CmdIndex> cmd_validation_queue;
  for (auto cmd_index : no_dependencies_) {
    cmd_validation_queue.push(cmd_index);
  }

  while (!cmd_validation_queue.empty()) {
    const auto &cmd = Command(cmd_validation_queue.front());
    cmd_validation_queue.pop();

    if (cmd.op_code == Op::Call) {
      Function *fn_type = cmd.args.back().value.as<Func *>()->type;
      i32 num_fn_args   = static_cast<i32>(
          fn_type->is<Tuple>() ? ptr_cast<Tuple>(fn_type)->entries.size() : 1);

      for (i32 i = 0; i < num_fn_args; ++i) {
        const auto &arg = cmd.args[i];

        const auto &property =
           cmd.args.back().value.as<Func *>()->properties_[Register(i)];
        if (property == nullptr) { continue; }

        if (arg.value.is<Register>()) {
          if (properties_[arg.value.as<Register>()]->Implies(property.get())) {
            goto next_prop;
          }

          ++num_errors_;
          LogError::FailedPrecondition(*property);

        } else {
          switch (property->Validate(arg)) {
          case Validity::Always: continue;
          default:
            ++num_errors_;
            LogError::FailedPrecondition(*property);
            break;
          }
        }
      next_prop:;
      }

      validation_queue->push(cmd.args.back().value.as<IR::Func *>());
    } else if (!cmd.result.value.is<Register>()) {
      continue;
    } else {
      auto &property = properties_[cmd.result.value.as<Register>()];
      auto new_prop  = cmd.MakeProperty(this);
      // TODO this equality check is not correct.
      if (new_prop != property) {
        property = std::move(new_prop);
        for (auto index : references_[cmd.result.value.as<Register>()]) {
          cmd_validation_queue.push(index);
        }
      }
    }
  }

  return num_errors_;
}
} // namespace IR
