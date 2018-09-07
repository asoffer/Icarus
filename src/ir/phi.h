#ifndef ICARUS_IR_PHI_H
#define ICARUS_IR_PHI_H

#include <unordered_map>
#include "ir/val.h"
#include "ir/func.h"

namespace IR {
CmdIndex Phi(type::Type const *);

Val MakePhi(CmdIndex phi_index,
            const std::unordered_map<BlockIndex, IR::Val> &val_map);

template <typename T>
RegisterOr<T> MakePhi(CmdIndex phi_index,
                      std::unordered_map<BlockIndex, RegisterOr<T>> val_map) {
  auto &cmd = IR::Func::Current->Command(phi_index);

  auto phi_args  = std::make_unique<PhiArgs<T>>();
  phi_args->map_ = std::move(val_map);

  if constexpr (std::is_same_v<T, bool>) {
    cmd.op_code_  = Op::PhiBool;
    cmd.phi_bool_ = Cmd::PhiBool::Make(phi_args.get());
  } else if constexpr (std::is_same_v<T, char>) {
    cmd.op_code_  = Op::PhiChar;
    cmd.phi_char_ = Cmd::PhiChar::Make(phi_args.get());
  } else if constexpr (std::is_same_v<T, i32>) {
    cmd.op_code_ = Op::PhiInt;
    cmd.phi_int_ = Cmd::PhiInt::Make(phi_args.get());
  } else if constexpr (std::is_same_v<T, double>) {
    cmd.op_code_  = Op::PhiReal;
    cmd.phi_real_ = Cmd::PhiReal::Make(phi_args.get());
  } else if constexpr (std::is_same_v<T, type::Type const *>) {
    cmd.op_code_  = Op::PhiType;
    cmd.phi_type_ = Cmd::PhiType::Make(phi_args.get());
  } else if constexpr (std::is_same_v<T, IR::Addr>) {
    cmd.op_code_  = Op::PhiAddr;
    cmd.phi_addr_ = Cmd::PhiAddr::Make(phi_args.get());
  }
  IR::Func::Current->block(BasicBlock::Current)
      .phi_args_.push_back(std::move(phi_args));
  return cmd.result;
}
}  // namespace IR

#endif // ICARUS_IR_PHI_H
