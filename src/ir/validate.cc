#include "ir.h"

#include "../error_log.h"

namespace IR {
int Func::ValidateCalls(std::queue<IR::Func *> *validation_queue) {
  if (num_errors_ >= 0) { return num_errors_; }

  std::queue<Register> register_validation_queue;
  for (const auto& kv : reg_map_) {
    register_validation_queue.push(kv.first);
  }

  while (!register_validation_queue.empty()) {
    Register curr_reg = register_validation_queue.front();
    const auto &cmd   = Command(curr_reg);
    register_validation_queue.pop();

    // TODO only do this if there's an update.
    for (auto reg : reg_references_[curr_reg]) {
      register_validation_queue.push(reg);
    }

    switch (cmd.op_code) {
    case Op::Print: break;
    case Op::Call:
      validation_queue->push(cmd.args.back().value.as<IR::Func *>());
      break;
    case Op::Validate:
      if (cmd.args[0].value.is<Register>()) {
        LOG << cmd.args[0].value.as<Register>();
      } else if (cmd.args[0].value.is<Argument>()) {
        LOG << cmd.args[0].value.as<Argument>();
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
    default:; // TODO
    }
  }

  return num_errors_;
}
} // namespace IR
