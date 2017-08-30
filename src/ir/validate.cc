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
    cmd.dump(0);
/*
    // TODO only do this if there's an update.
    for (auto cmd_index : references_[cmd.result.value.as<Register>()]) {
      cmd_validation_queue.push(cmd_index);
    }
*/
    switch (cmd.op_code) {
    case Op::Print: break;
    case Op::Call:
      validation_queue->push(cmd.args.back().value.as<IR::Func *>());
      break;
    case Op::Validate:
      if (cmd.args[0].value.is<Register>()) {
        LOG << cmd.args[0].value.as<Register>();
      } else {
        for (const auto &property :
             *cmd.args[1]
                  .value.as<const std::vector<std::unique_ptr<Property>> *>()) {
          switch (property->Validate(cmd.args[0])) {
          case Validity::Always: continue;
          default:
            ++num_errors_;
            LogError::FailedPrecondition(*property);
            break;
          }
        }
      }
    default:;
    }
  }

  return num_errors_;
}
} // namespace IR
