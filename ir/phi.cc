#include "ir/phi.h"

#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/struct.h"

namespace ir {
template <typename T>
static std::unordered_map<BlockIndex, RegisterOr<T>> ConvertMap(
    std::unordered_map<BlockIndex, Results> const &val_map) {
  std::unordered_map<BlockIndex, RegisterOr<T>> result;

  for (auto const & [ block, val ] : val_map) {
    result.emplace(block, val.template get<T>(0));
  }

  return result;
}

template <typename T>
static std::unique_ptr<PhiArgs<T>> MakePhiArgs(
    std::unordered_map<BlockIndex, Results> const &val_map) {
  auto phi_args = std::make_unique<PhiArgs<T>>();
  for (auto const &[block, val] : val_map) {
    phi_args->map_.emplace(block, val.template get<T>(0));
  }
  return phi_args;
}

CmdIndex Phi(type::Type const *t) {
  CmdIndex cmd_index{
      BasicBlock::Current,
      static_cast<int32_t>(Func::Current->block(BasicBlock::Current).cmds_.size())};
  ASSERT_NOT_NULL(Func::Current)
      ->block(BasicBlock::Current)
      .cmds_.emplace_back(t, Op::Death);
  return cmd_index;
}

ir::Results MakePhi(type::Type const *t, CmdIndex phi_index,
            std::unordered_map<BlockIndex, ir::Results> const &val_map) {
  auto &cmd = ir::Func::Current->Command(phi_index);

  if (t->is_big()) {
    return ir::Results{
        MakePhi<ir::Addr>(phi_index, ConvertMap<ir::Addr>(val_map))};
  }

  return type::Apply(t, [&](auto type_holder) {
    using T = typename decltype(type_holder)::type;
    if constexpr (std::is_same_v<T, type::Struct const *>) {
      NOT_YET();
      return Results{};
    } else if constexpr (std::is_same_v<T, ir::Addr>) {
      return ir::Results{MakePhi<T>(phi_index, ConvertMap<T>(val_map))};
    } else if constexpr (std::is_same_v<T, ir::EnumVal>) {
      ASSERT(val_map.size() != 0u);
      return Results{MakePhi<T>(phi_index, ConvertMap<T>(val_map))};
    } else if constexpr (std::is_same_v<T, ir::FlagsVal>) {
      ASSERT(val_map.size() != 0u);
      return Results{MakePhi<T>(phi_index, ConvertMap<T>(val_map))};
    } else if constexpr (std::is_same_v<T, BlockSequence>) {
      auto phi_args  = MakePhiArgs<BlockSequence>(val_map);
      cmd.op_code_   = Op::PhiBlock;
      cmd.phi_block_ = phi_args.get();
      ir::Func::Current->block(BasicBlock::Current)
          .phi_args_.push_back(std::move(phi_args));
      return Results{cmd.result};
    } else {
      return Results{MakePhi<T>(phi_index, ConvertMap<T>(val_map))};
    }
  });
}

}  // namespace ir
