#ifndef ICARUS_IR_PHI_H
#define ICARUS_IR_PHI_H

#include "absl/container/flat_hash_map.h"
#include "ir/compiled_fn.h"
#include "ir/reg.h"

namespace ir {
CmdIndex Phi(type::Type const *);

ir::Results MakePhi(type::Type const *type, CmdIndex phi_index,
            absl::flat_hash_map<BlockIndex, ir::Results> const &val_map);

template <typename T>
RegOr<T> MakePhi(CmdIndex phi_index,
                      absl::flat_hash_map<BlockIndex, RegOr<T>> val_map) {
  auto &cmd = ir::CompiledFn::Current->Command(phi_index);

  auto phi_args  = std::make_unique<PhiArgs<T>>();
  phi_args->map_ = std::move(val_map);

  if constexpr (std::is_same_v<T, bool>) {
    cmd.op_code_  = Op::PhiBool;
    cmd.phi_bool_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, int8_t>) {
    cmd.op_code_ = Op::PhiInt8;
    cmd.phi_i8_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, int16_t>) {
    cmd.op_code_ = Op::PhiInt16;
    cmd.phi_i16_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, int32_t>) {
    cmd.op_code_ = Op::PhiInt32;
    cmd.phi_i32_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, int64_t>) {
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
  } else if constexpr (std::is_same_v<T, ir::EnumVal>) {
    cmd.op_code_  = Op::PhiEnum;
    cmd.phi_enum_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, ir::FlagsVal>) {
    cmd.op_code_   = Op::PhiFlags;
    cmd.phi_flags_ = phi_args.get();
  } else if constexpr (std::is_same_v<T, ir::AnyFunc>) {
    cmd.op_code_  = Op::PhiFunc;
    cmd.phi_func_ = phi_args.get();
  }

  ir::CompiledFn::Current->block(BasicBlock::Current)
      .phi_args_.push_back(std::move(phi_args));
  return cmd.result;
}
}  // namespace ir

#endif  // ICARUS_IR_PHI_H
