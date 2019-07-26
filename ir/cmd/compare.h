#ifndef ICARUS_IR_CMD_COMPARE_H
#define ICARUS_IR_CMD_COMPARE_H

#include <functional>
#include <optional>
#include <type_traits>

#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "type/util.h"

namespace ir {

template <uint8_t Index, typename Fn>
struct CompareCmd {
  using fn_type                  = Fn;
  constexpr static uint8_t index = Index;

  struct control_bits {
    uint8_t reg0 : 1;
    uint8_t reg1 : 1;
    uint8_t primitive_type : 6;
  };

  template <typename T>
  static control_bits MakeControlBits(bool reg0, bool reg1) {
    control_bits result;
    result.reg0           = reg0;
    result.reg1           = reg1;
    result.primitive_type = PrimitiveIndex<T>();
    return result;
  }

  template <typename T>
  static T Apply(base::untyped_buffer::iterator* iter, bool reg0, bool reg1,
                 backend::ExecContext* ctx) {
    if constexpr (std::is_arithmetic_v<T> && !std::is_same_v<T, bool>) {
      auto lhs = reg0 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
      auto rhs = reg1 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
      return Fn{}(lhs,rhs);
    } else {
      return T{};
    }
  }

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
                                           std::vector<Addr> const& ret_slots,
                                           backend::ExecContext* ctx) {
    auto& frame = ctx->call_stack.top();
    auto ctrl   = iter->read<control_bits>();
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using type = typename std::decay_t<decltype(tag)>::type;
      frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
                      Apply<type>(iter, ctrl.reg0, ctrl.reg1, ctx));
    });
    return std::nullopt;
  }
};

using LtCmd  = CompareCmd<6, std::less<>>;
using LeCmd  = CompareCmd<7, std::less_equal<>>;
using EqCmd  = CompareCmd<8, std::equal_to<>>;
using NeCmd  = CompareCmd<9, std::not_equal_to<>>;
using GeCmd  = CompareCmd<10, std::greater_equal<>>;
using GtCmd  = CompareCmd<11, std::greater<>>;

namespace internal {
template <typename CmdType>
struct CompareHandler {
  template <typename... Args,
            typename std::enable_if_t<!std::conjunction_v<std::is_same<
                Args, RegOr<UnwrapTypeT<Args>>>...>>* = nullptr>
  auto operator()(Args... args) const {
    return operator()(
        RegOr<UnwrapTypeT<Args>>(std::forward<Args>(args))...);
  }

  template <typename T>
  RegOr<bool> operator()(RegOr<T> lhs, RegOr<T> rhs) const {
    auto& blk = GetBlock();
    using fn_type     = typename CmdType::fn_type;
    if (!lhs.is_reg_ && !rhs.is_reg_) { return fn_type{}(lhs.val_, rhs.val_); }

    blk.cmd_buffer_.append_index<CmdType>();
    blk.cmd_buffer_.append(
        CmdType::template MakeControlBits<T>(lhs.is_reg_, rhs.is_reg_));

    if (lhs.is_reg_) {
      blk.cmd_buffer_.append(lhs.reg_);
    } else {
      blk.cmd_buffer_.append(lhs.val_);
    }
    if (rhs.is_reg_) {
      blk.cmd_buffer_.append(rhs.reg_);
    } else {
      blk.cmd_buffer_.append(rhs.val_);
    }

    Reg result = MakeResult(type::Get<T>());
    blk.cmd_buffer_.append(result);
    DEBUG_LOG("compare")(blk.cmd_buffer_.to_string());
    return result;
  }
};

}  // namespace internal

constexpr inline auto Lt = internal::CompareHandler<LtCmd>{};
constexpr inline auto Le = internal::CompareHandler<LeCmd>{};
constexpr inline auto Eq = internal::CompareHandler<EqCmd>{};
constexpr inline auto Ne = internal::CompareHandler<NeCmd>{};
constexpr inline auto Ge = internal::CompareHandler<GeCmd>{};
constexpr inline auto Gt = internal::CompareHandler<GtCmd>{};

}  // namespace ir

#endif  // ICARUS_IR_CMD_COMPARE_H
