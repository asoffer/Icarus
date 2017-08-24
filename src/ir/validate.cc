#include "ir.h"

#include "../error_log.h"

namespace IR {
int Block::ValidateCalls(std::queue<IR::Func *> *validation_queue) {
  int num_errors = 0;
  // TODO track data in registers
  for (auto &cmd : cmds_) {
    switch (cmd.op_code) {
    case Op::Print: break;
    case Op::Call:
      validation_queue->push(cmd.args.back().value.as<IR::Func *>());
      break;
    case Op::Validate:
      if (cmd.args[0].value.is<Register>()) {
      } else if (cmd.args[0].value.is<Argument>()) {
      } else {
        for (const auto &property :
             *cmd.args[1]
                  .value.as<const std::vector<std::unique_ptr<Property>> *>()) {
          switch (property->Validate(cmd.args[0])) {
          case Validity::Always: continue;
          default:
            ++num_errors;
            LogError::FailedPrecondition(*property);
            break;
          }
        }
      }
    default:; // TODO
    }
  }
  return num_errors;
}

int Func::ValidateCalls(std::queue<IR::Func *> *validation_queue) {
  if (num_errors_ >= 0) { return num_errors_; }

  // TODO track data across blocks
  num_errors_ = 0;
  for (auto &block : blocks_) {
    num_errors_ += block.ValidateCalls(validation_queue);
  }
  return num_errors_;
}
} // namespace IR
