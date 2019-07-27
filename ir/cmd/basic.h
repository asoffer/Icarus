#ifndef ICARUS_IR_CMD_BASIC_H
#define ICARUS_IR_CMD_BASIC_H

#include <functional>
#include <optional>

#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "type/util.h"

namespace ir {

template <uint8_t Index, typename Fn, typename... SupportedTypes>
struct BinaryCmd {
  using fn_type                  = Fn;
  constexpr static uint8_t index = Index;

  template <typename T>
  static constexpr bool IsSupported() {
    return std::disjunction_v<std::is_same<T, SupportedTypes>...>;
  }

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

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
                                           std::vector<Addr> const& ret_slots,
                                           backend::ExecContext* ctx) {
    auto& frame = ctx->call_stack.top();
    auto ctrl   = iter->read<control_bits>();
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using type = typename std::decay_t<decltype(tag)>::type;
      auto result = Apply<type>(iter, ctrl.reg0, ctrl.reg1, ctx);
      frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()), result);
    });
    return std::nullopt;
  }

 private:
  template <typename T>
  static auto Apply(base::untyped_buffer::iterator* iter, bool reg0, bool reg1,
                    backend::ExecContext* ctx) {
    if constexpr (IsSupported<T>()) {
      auto lhs = reg0 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
      auto rhs = reg1 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
      return Fn{}(lhs, rhs);
    } else {
      return T{};
    }
  }
};

using AddCmd = BinaryCmd<1, std::plus<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, float, double>;
using SubCmd = BinaryCmd<2, std::minus<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, float, double>;
using MulCmd = BinaryCmd<3, std::multiplies<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, float, double>;
using DivCmd = BinaryCmd<4, std::divides<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, float, double>;
using ModCmd = BinaryCmd<5, std::modulus<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t>;
using LtCmd  = BinaryCmd<6, std::less<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, FlagsVal>;
using LeCmd  = BinaryCmd<7, std::less_equal<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, FlagsVal>;
using EqCmd  = BinaryCmd<8, std::equal_to<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, FlagsVal, EnumVal>;
using NeCmd  = BinaryCmd<9, std::not_equal_to<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, FlagsVal, EnumVal>;
using GeCmd  = BinaryCmd<10, std::greater_equal<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, FlagsVal>;
using GtCmd  = BinaryCmd<11, std::greater<>,  //
                         int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                         uint32_t, uint64_t, FlagsVal>;

namespace internal {
template <typename CmdType>
struct BinaryHandler {
  template <typename... Args,
            typename std::enable_if_t<!std::conjunction_v<std::is_same<
                Args, RegOr<UnwrapTypeT<Args>>>...>>* = nullptr>
  auto operator()(Args... args) const {
    return operator()(
        RegOr<UnwrapTypeT<Args>>(std::forward<Args>(args))...);
  }

  template <typename T>
  auto operator()(RegOr<T> lhs, RegOr<T> rhs) const {
    auto& blk = GetBlock();
    using fn_type     = typename CmdType::fn_type;
    using result_type = decltype(fn_type{}(lhs.val_, rhs.val_));
    if constexpr (CmdType::template IsSupported<T>()) {
      if (!lhs.is_reg_ && !rhs.is_reg_) {
        return RegOr<result_type>{fn_type{}(lhs.val_, rhs.val_)};
      }
    }

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
    return RegOr<result_type>{result};
  }
};

}  // namespace internal

constexpr inline auto Add = internal::BinaryHandler<AddCmd>{};
constexpr inline auto Sub = internal::BinaryHandler<SubCmd>{};
constexpr inline auto Mul = internal::BinaryHandler<MulCmd>{};
constexpr inline auto Div = internal::BinaryHandler<DivCmd>{};
constexpr inline auto Mod = internal::BinaryHandler<ModCmd>{};
constexpr inline auto Lt  = internal::BinaryHandler<LtCmd>{};
constexpr inline auto Le  = internal::BinaryHandler<LeCmd>{};
constexpr inline auto Eq  = internal::BinaryHandler<EqCmd>{};
constexpr inline auto Ne  = internal::BinaryHandler<NeCmd>{};
constexpr inline auto Ge  = internal::BinaryHandler<GeCmd>{};
constexpr inline auto Gt  = internal::BinaryHandler<GtCmd>{};

template <uint8_t Index, typename Fn, typename... SupportedTypes>
struct UnaryCmd {
  using fn_type                                 = Fn;
  constexpr static uint8_t index                = Index;

  template <typename T>
  static constexpr bool IsSupported() {
    return std::disjunction_v<std::is_same<T, SupportedTypes>...>;
  }

  struct control_bits {
    uint8_t reg0 : 1;
    uint8_t primitive_type : 6;
  };

  template <typename T>
  static control_bits MakeControlBits(bool reg0) {
    control_bits result;
    result.reg0           = reg0;
    result.primitive_type = PrimitiveIndex<T>();
    return result;
  }

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
                                           std::vector<Addr> const& ret_slots,
                                           backend::ExecContext* ctx) {
    auto& frame = ctx->call_stack.top();
    auto ctrl = iter->read<control_bits>();
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using type = typename std::decay_t<decltype(tag)>::type;
      auto result = Apply<type>(iter, ctrl.reg0, ctx);
      frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()), result);
    });
    return std::nullopt;
  }

 private:
  template <typename T>
  static auto Apply(base::untyped_buffer::iterator* iter, bool reg0,
                    backend::ExecContext* ctx) {
    if constexpr (IsSupported<T>()) {
      return Fn{}(reg0 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>());
    } else {
      return T{};
    }
  }
};

using NegCmd = UnaryCmd<12, std::negate<>,  //
                        int8_t, int16_t, int32_t, int64_t, float, double>;
using NotCmd = UnaryCmd<15, std::logical_not<>,  //
                        bool>;

namespace internal {
template <typename CmdType>
struct UnaryHandler {
  template <typename... Args,
            typename std::enable_if_t<!std::conjunction_v<std::is_same<
                Args, RegOr<UnwrapTypeT<Args>>>...>>* = nullptr>
  auto operator()(Args... args) const {
    return operator()(
        RegOr<UnwrapTypeT<Args>>(std::forward<Args>(args))...);
  }

  template <typename T>
  auto operator()(RegOr<T> operand) const {
    auto& blk = GetBlock();
    using fn_type     = typename CmdType::fn_type;
    using result_type = decltype(fn_type{}(operand.val_));
    if constexpr (CmdType::template IsSupported<T>()) {
      if (!operand.is_reg_) {
        return RegOr<result_type>{fn_type{}(operand.val_)};
      }
    }

    blk.cmd_buffer_.append_index<CmdType>();
    blk.cmd_buffer_.append(
        CmdType::template MakeControlBits<T>(operand.is_reg_));

    if (operand.is_reg_) {
      blk.cmd_buffer_.append(operand.reg_);
    } else {
      blk.cmd_buffer_.append(operand.val_);
    }

    Reg result = MakeResult(type::Get<T>());
    blk.cmd_buffer_.append(result);
    return RegOr<result_type>{result};
  }
};

}  // namespace internal

constexpr inline auto Neg = internal::UnaryHandler<NegCmd>{};
constexpr inline auto Not = internal::UnaryHandler<NotCmd>{};
}  // namespace ir

#endif  // ICARUS_IR_CMD_BASIC_H
