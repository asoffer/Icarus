#ifndef ICARUS_IR_PHI_H
#define ICARUS_IR_PHI_H

#include <unordered_map>
#include "ir/func.h"
#include "ir/val.h"

namespace ir {
CmdIndex Phi(type::Type const *);

Val MakePhi(CmdIndex phi_index,
            const std::unordered_map<BlockIndex, ir::Val> &val_map);

template <typename T>
RegisterOr<T> MakePhi(CmdIndex phi_index,
                      std::unordered_map<BlockIndex, RegisterOr<T>> val_map) {
  auto &cmd = ir::Func::Current->Command(phi_index);

  auto phi_args  = std::make_unique<PhiArgs<T>>();
  phi_args->map_ = std::move(val_map);

  if constexpr (std::is_same_v<T, bool>) {
    cmd.op_code_  = Op::PhiBool;
    cmd.phi_bool_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, i8>) {
    cmd.op_code_ = Op::PhiInt8;
    cmd.phi_i8_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, i16>) {
    cmd.op_code_ = Op::PhiInt16;
    cmd.phi_i16_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, i32>) {
    cmd.op_code_ = Op::PhiInt32;
    cmd.phi_i32_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, i64>) {
    cmd.op_code_ = Op::PhiInt64;
    cmd.phi_i64_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, float>) {
    cmd.op_code_     = Op::PhiFloat32;
    cmd.phi_float32_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, double>) {
    cmd.op_code_     = Op::PhiFloat64;
    cmd.phi_float64_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, type::Type const *>) {
    cmd.op_code_  = Op::PhiType;
    cmd.phi_type_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, ir::Addr>) {
    cmd.op_code_  = Op::PhiAddr;
    cmd.phi_addr_ = phi_args.get();
  }
  ir::Func::Current->block(BasicBlock::Current)
      .phi_args_.push_back(std::move(phi_args));
  return cmd.result;
}
}  // namespace ir

#endif  // ICARUS_IR_PHI_H
