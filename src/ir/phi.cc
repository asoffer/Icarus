#include "ir/phi.h"

#include "type/pointer.h"
#include "type/struct.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/char_buffer.h"

namespace ir {
template <typename T>
static std::unordered_map<BlockIndex, RegisterOr<T>> ConvertMap(
    const std::unordered_map<BlockIndex, Val> &val_map) {
  std::unordered_map<BlockIndex, RegisterOr<T>> result;

  for (const auto & [ block, val ] : val_map) {
    result.emplace(block, val.template reg_or<T>());
  }

  return result;
}

template <typename T>
static std::unique_ptr<PhiArgs<T>> MakePhiArgs(
    const std::unordered_map<BlockIndex, ir::Val> &val_map) {
  auto phi_args = std::make_unique<PhiArgs<T>>();
  for (const auto & [ block, val ] : val_map) {
    phi_args->map_.emplace(block, val.template reg_or<T>());
  }
  return phi_args;
}

CmdIndex Phi(type::Type const *t) {
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<i32>(Func::Current->block(BasicBlock::Current).cmds_.size())};
  ASSERT_NOT_NULL(Func::Current)
      ->block(BasicBlock::Current)
      .cmds_.emplace_back(t, Op::Death);
  return cmd_index;
}

Val MakePhi(CmdIndex phi_index,
            const std::unordered_map<BlockIndex, ir::Val> &val_map) {
  auto &cmd      = ir::Func::Current->Command(phi_index);
  auto *cmd_type = val_map.begin()->second.type;

  return type::Apply(cmd_type, [&](auto type_holder) {
    using T = typename decltype(type_holder)::type;
    if constexpr (std::is_same_v<T, ir::Addr>) {
      return ir::ValFrom(
          MakePhi<ir::Addr>(phi_index, ConvertMap<ir::Addr>(val_map)),
          &cmd_type->as<type::Pointer>());
    } else if constexpr (std::is_same_v<T, BlockSequence>) {
      auto phi_args  = MakePhiArgs<BlockSequence>(val_map);
      cmd.op_code_   = Op::PhiBlock;
      cmd.phi_block_ = phi_args.get();
      ir::Func::Current->block(BasicBlock::Current)
          .phi_args_.push_back(std::move(phi_args));
      return ir::Val::Reg(cmd.result, val_map.begin()->second.type);
    } else {
      return ir::ValFrom(MakePhi<T>(phi_index, ConvertMap<T>(val_map)));
    }
  });
}

}  // namespace ir
