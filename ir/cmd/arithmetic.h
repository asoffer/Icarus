#ifndef ICARUS_IR_CMD_ARITHMETIC_H
#define ICARUS_IR_CMD_ARITHMETIC_H

#include <functional>
#include <optional>

#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "type/util.h"

namespace ir {

template <uint8_t Index, typename Fn, bool SupportsFloatingPoint = true>
struct BinaryArithmeticCmd {
  using fn_type                                 = Fn;
  constexpr static bool supports_floating_point = SupportsFloatingPoint;
  constexpr static uint8_t index                = Index;

  struct control_bits {
    unsigned reg0 : 1;
    unsigned reg1 : 1;
    unsigned primitive_type : 6;
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
    if constexpr (std::is_arithmetic_v<T> && !std::is_same_v<T, bool> &&
                  (SupportsFloatingPoint || !std::is_floating_point_v<T>)) {
      return Fn{}(reg0 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>(),
                  reg1 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>());
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

using AddCmd = BinaryArithmeticCmd<1, std::plus<>>;
using SubCmd = BinaryArithmeticCmd<2, std::minus<>>;
using MulCmd = BinaryArithmeticCmd<3, std::multiplies<>>;
using DivCmd = BinaryArithmeticCmd<4, std::divides<>>;
using ModCmd = BinaryArithmeticCmd<5, std::modulus<>, false>;

namespace internal {
template <typename CmdType>
struct BinaryArithmeticHandler {
  template <typename T>
  RegisterOr<T> operator()(TypedRegister<T> lhs, T rhs) const {
    return operator()(static_cast<RegisterOr<T>>(lhs), RegisterOr<T>(rhs));
  }

  template <typename T>
  RegisterOr<T> operator()(RegisterOr<T> lhs, T rhs) const {
    return operator()(lhs, RegisterOr<T>(rhs));
  }

  template <typename T>
  RegisterOr<T> operator()(TypedRegister<T> lhs, RegisterOr<T> rhs) const {
    return operator()(static_cast<RegisterOr<T>>(lhs), rhs);
  }

  template <typename T>
  RegisterOr<T> operator()(RegisterOr<T> lhs, TypedRegister<T> rhs) const {
    return operator()(lhs, static_cast<RegisterOr<T>>(rhs));
  }

  template <typename T>
  RegisterOr<T> operator()(TypedRegister<T> lhs, TypedRegister<T> rhs) const {
    return operator()(static_cast<RegisterOr<T>>(lhs),
                      static_cast<RegisterOr<T>>(rhs));
  }

  template <typename T>
  RegisterOr<T> operator()(RegisterOr<T> lhs, RegisterOr<T> rhs) const {
    auto& blk = GetBlock();
    blk.cmd_buffer_.append_index<CmdType>();
    blk.cmd_buffer_.append(
        CmdType::template MakeControlBits<T>(lhs.is_reg_, rhs.is_reg_));

    if constexpr (CmdType::supports_floating_point ||
                  !std::is_floating_point_v<T>) {
      if (!lhs.is_reg_ && !rhs.is_reg_) {
        using fn_type = typename CmdType::fn_type;
        return fn_type{}(lhs.val_, rhs.val_);
      }
    }

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
    DEBUG_LOG("arithmetic")(blk.cmd_buffer_.to_string());
    return result;
  }
};

}  // namespace internal

inline auto Add = internal::BinaryArithmeticHandler<AddCmd>{};
inline auto Sub = internal::BinaryArithmeticHandler<SubCmd>{};
inline auto Mul = internal::BinaryArithmeticHandler<MulCmd>{};
inline auto Div = internal::BinaryArithmeticHandler<DivCmd>{};
inline auto Mod = internal::BinaryArithmeticHandler<ModCmd>{};

}  // namespace ir

#endif  // ICARUS_IR_CMD_ARITHMETIC_H
