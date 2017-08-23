#include "ir.h"

namespace IR {
void Block::ValidateCalls() {
  // TODO track data in registers
  for (auto &cmd : cmds_) {
    switch (cmd.op_code) {
    case Op::Print: break;
    case Op::Validate:
      if (cmd.args[0].value.is<Register>()) {
      } else if (cmd.args[0].value.is<Argument>()) {
      } else {
        for (const auto &property :
             *cmd.args[1]
                  .value.as<const std::vector<std::unique_ptr<Property>> *>()) {
          switch (property->Validate(cmd.args[0])) {
          case Validity::Always: continue;
          default: LOG << "Error!";
          }
        }
      }
    default:; // TODO
    }
  }
}

void Func::ValidateCalls() {
  if (has_been_validated_) { return; }
  // Do this immediately to avoid recursive validation.
  has_been_validated_ = true;

  // TODO track data across blocks
  for (auto &block : blocks_) { block.ValidateCalls(); }
}
} // namespace IR
