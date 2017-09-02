#include "ir.h"

#include "../error_log.h"

namespace IR {
int Func::ValidateCalls(std::queue<IR::Func *> *validation_queue) {
  if (num_errors_ >= 0) { return num_errors_; }

  std::queue<CmdIndex> cmd_validation_queue;
  for (auto cmd_index : no_dependencies_) {
    cmd_validation_queue.push(cmd_index);
  }

  while (!cmd_validation_queue.empty()) {
    const auto &cmd = Command(cmd_validation_queue.front());
    cmd_validation_queue.pop();

    switch (cmd.op_code) {
    case Op::Add: {
      auto &preconditions = preconditions_[cmd.result.value.as<Register>()];
      std::vector<std::unique_ptr<Property>> updated_preconditions;

      if (cmd.args[0].value.is<Register>()) {
        if (cmd.args[1].value.is<Register>()) {
          NOT_YET();
        } else {
          for (const auto &prop :
               preconditions_[cmd.args[0].value.as<Register>()]) {
            auto new_prop = prop->Add(cmd.args[1]);
            if (new_prop != nullptr) {
              updated_preconditions.push_back(std::move(new_prop));
            }
          }
        }
      }

      // TODO this equality check is not correct.
      if (updated_preconditions != preconditions) {
        preconditions = std::move(updated_preconditions);
        for (auto cmd_index : references_[cmd.result.value.as<Register>()]) {
          cmd_validation_queue.push(cmd_index);
        }
      }
    } break;
    case Op::Print: break;
    case Op::Call: {
      Function *fn_type = cmd.args.back().value.as<Func *>()->type;
      size_t num_fn_args =
          fn_type->is<Tuple>() ? ptr_cast<Tuple>(fn_type)->entries.size() : 1;

      for (size_t i = 0; i < num_fn_args; ++i) {
        const auto &arg = cmd.args[i];

        for (const auto &property : cmd.args.back()
                                        .value.as<Func *>()
                                        ->preconditions_[static_cast<i32>(i)]) {
          if (arg.value.is<Register>()) {
            for (const auto &known_prop :
                 preconditions_[arg.value.as<Register>()]) {
              if (known_prop->Implies(property.get())) { goto next_prop; }
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
      }

      validation_queue->push(cmd.args.back().value.as<IR::Func *>());
    } break;
    default:;
    }
  }

  return num_errors_;
}
} // namespace IR
