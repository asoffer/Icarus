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
/*
    // TODO only do this if there's an update.
    for (auto cmd_index : references_[cmd.result.value.as<Register>()]) {
      cmd_validation_queue.push(cmd_index);
    }
*/
    switch (cmd.op_code) {
    case Op::Add: {
      auto &preconditions = preconditions_[cmd.result.value.as<Register>()];
      preconditions.clear();
      if (cmd.args[0].value.is<Register>()) {
        if (cmd.args[1].value.is<Register>()) {
          NOT_YET();
        } else {
          for (const auto &prop :
               preconditions_[cmd.args[0].value.as<Register>()]) {
            auto new_prop = prop->Add(cmd.args[1]);
            if (new_prop != nullptr) {
              preconditions.push_back(std::move(new_prop));
            }
          }
        }
      }
    } break;
    case Op::Print: break;
    case Op::Call:
      for (const auto &arg_and_precondition :
           cmd.args.back().value.as<Func *>()->preconditions_) {
        const auto &arg = cmd.args[arg_and_precondition.first.value];
        for (const auto &property : arg_and_precondition.second) {
          if (arg.value.is<Register>()) {
            LOG << arg.to_string();
          } else {
            switch (property->Validate(arg)) {
            case Validity::Always: continue;
            default:
              ++num_errors_;
              LogError::FailedPrecondition(*property);
              break;
            }
          }
        }
      }

      validation_queue->push(cmd.args.back().value.as<IR::Func *>());
      break;
    default:;
    }
  }

  return num_errors_;
}
} // namespace IR
