#include "ir.h"

#include "../error_log.h"
#include "property.h"

// TODO repeated in generate_postcondition
static IR::Register RetToReg(IR::ReturnValue v) {
  return IR::Register{-v.value - 1};
}

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
      auto *called_fn   = cmd.args.back().value.as<Func *>();
      Function *fn_type = called_fn->type;
      i32 num_fn_args   = static_cast<i32>(
          fn_type->is<Tuple>() ? ptr_cast<Tuple>(fn_type)->entries.size() : 1);

      for (i32 i = 0; i < num_fn_args; ++i) {
        const auto &arg = cmd.args[i];

        const auto &property = called_fn->properties_[Register(i)];
        if (property == nullptr) { continue; }

        if (arg.value.is<Register>()) {
          const auto &prop = properties_[arg.value.as<Register>()];
          if (prop != nullptr && prop->Implies(property.get())) {
            goto next_prop;
          }

          // Failure here could mean you know it's never true or that you can't
          // prove it's true.

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

      if (fn_type->output != Void) {
        // TODO multiple return values?
        const auto &ensured_property = called_fn->properties_[Register(-1)];
        if (ensured_property != nullptr) {
          // TODO if this property has relationships to the input arguments, you
          // need to rectify those references too.
          // TODO make properties copyable (don't use unique_ptr). For now only
          // support bool properties and hand-craft the copying.
          if (ensured_property->is<property::BoolProperty>()) {
            properties_[cmd.result.value.as<Register>()] =
                std::make_unique<property::BoolProperty>(
                    ensured_property->as<property::BoolProperty>());
          } else {
            NOT_YET();
          }
        }
      }

      validation_queue->push(called_fn);
    } else if (cmd.op_code == Op::SetReturn) {
      const auto &ensured_property =
          properties_[RetToReg(cmd.args[0].value.as<ReturnValue>())];
      if (ensured_property != nullptr) { LOG << *ensured_property; }
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
