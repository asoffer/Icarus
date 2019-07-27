#ifndef ICARUS_IR_CMD_UTIL_H
#define ICARUS_IR_CMD_UTIL_H

#include <type_traits>

#include "base/util.h"
#include "ir/addr.h"
#include "ir/reg.h"

namespace type {
struct Type;
}  // namespace type

namespace ir {
using cmd_index_t = uint8_t;

template <typename T>
constexpr cmd_index_t PrimitiveIndex() {
  if constexpr (std::is_same_v<T, bool>) {
    return 0x08;
  } else if constexpr (std::is_same_v<T, float>) {
    return 0x09;
  } else if constexpr (std::is_same_v<T, double>) {
    return 0x0a;
  } else if constexpr (std::is_same_v<T, std::string_view>) {
    return 0x0b;
  } else if constexpr (std::is_same_v<T, type::Type const *>) {
    return 0x0c;
  } else if constexpr (std::is_same_v<T, Addr>) {
    return 0x0d;
  } else if constexpr (std::is_same_v<T, EnumVal>) {
    return 0x0e;
  } else if constexpr (std::is_same_v<T, FlagsVal>) {
    return 0x0f;
  } else if constexpr (std::is_integral_v<T>) {
    return base::Log2(sizeof(T)) * 2 + std::is_signed_v<T>;
  } else {
    UNREACHABLE();
  }
}

template <typename Fn>
auto PrimitiveDispatch(cmd_index_t primitive_type, Fn&& fn) {
  switch (primitive_type) {
    case PrimitiveIndex<uint8_t>():
      return std::forward<Fn>(fn)(base::Tag<uint8_t>{});
    case PrimitiveIndex<int8_t>():
      return std::forward<Fn>(fn)(base::Tag<int8_t>{});
    case PrimitiveIndex<uint16_t>():
      return std::forward<Fn>(fn)(base::Tag<uint8_t>{});
    case PrimitiveIndex<int16_t>():
      return std::forward<Fn>(fn)(base::Tag<int16_t>{});
    case PrimitiveIndex<uint32_t>():
      return std::forward<Fn>(fn)(base::Tag<uint32_t>{});
    case PrimitiveIndex<int32_t>():
      return std::forward<Fn>(fn)(base::Tag<int32_t>{});
    case PrimitiveIndex<uint64_t>():
      return std::forward<Fn>(fn)(base::Tag<uint64_t>{});
    case PrimitiveIndex<int64_t>():
      return std::forward<Fn>(fn)(base::Tag<int64_t>{});
    case PrimitiveIndex<bool>(): return std::forward<Fn>(fn)(base::Tag<bool>{});
    case PrimitiveIndex<float>():
      return std::forward<Fn>(fn)(base::Tag<float>{});
    case PrimitiveIndex<double>():
      return std::forward<Fn>(fn)(base::Tag<double>{});
    case PrimitiveIndex<std::string_view>():
      return std::forward<Fn>(fn)(base::Tag<std::string_view>{});
    case PrimitiveIndex<type::Type const *>():
      return std::forward<Fn>(fn)(base::Tag<type::Type const *>{});
    case PrimitiveIndex<Addr>():
      return std::forward<Fn>(fn)(base::Tag<Addr>{});
    case PrimitiveIndex<EnumVal>():
      return std::forward<Fn>(fn)(base::Tag<EnumVal>{});
    case PrimitiveIndex<FlagsVal>():
      return std::forward<Fn>(fn)(base::Tag<FlagsVal>{});
  }
}

namespace internal {
template <typename T>
struct UnwrapType {
  using type = T;
};

template <typename T>
struct UnwrapType<RegOr<T>> {
  using type = T;
};

template <typename T>
struct UnwrapType<TypedRegister<T>> {
  using type = T;
};

template <typename T>
using UnwrapTypeT = typename UnwrapType<T>::type;

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

template <typename CmdType>
struct UnaryHandler {
  template <typename... Args,
            typename std::enable_if_t<!std::conjunction_v<
                std::is_same<Args, RegOr<UnwrapTypeT<Args>>>...>>* = nullptr>
  auto operator()(Args... args) const {
    return operator()(RegOr<UnwrapTypeT<Args>>(std::forward<Args>(args))...);
  }

  template <typename T>
  auto operator()(RegOr<T> operand) const {
    auto& blk         = GetBlock();
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
      using type  = typename std::decay_t<decltype(tag)>::type;
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

template <typename CmdType>
struct BinaryHandler {
  template <typename... Args,
            typename std::enable_if_t<!std::conjunction_v<
                std::is_same<Args, RegOr<UnwrapTypeT<Args>>>...>>* = nullptr>
  auto operator()(Args... args) const {
    return operator()(RegOr<UnwrapTypeT<Args>>(std::forward<Args>(args))...);
  }

  template <typename T>
  auto operator()(RegOr<T> lhs, RegOr<T> rhs) const {
    auto& blk         = GetBlock();
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

template <cmd_index_t Index, typename T, T (*Fn)(std::vector<T>)>
struct VariadicCmd {
  constexpr static cmd_index_t index = Index;
  using type                         = T;
  constexpr static auto* fn_ptr      = Fn;

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
                                           std::vector<Addr> const& ret_slots,
                                           backend::ExecContext* ctx) {
    uint16_t num    = iter->read<uint16_t>();
    uint8_t current = 0;

    std::vector<T> vals;
    vals.reserve(num);
    {
      std::vector<bool> bits;
      bits.reserve(num);
      for (uint16_t i = 0; i < num; ++i) {
        if (i % 8 == 0) { current = ReverseByte(iter->read<uint8_t>()); }
        bits.push_back(current & 1);
        current >>= 1;
      }

      for (bool b : bits) {
        vals.push_back(b ? ctx->resolve<T>(iter->read<Reg>())
                         : iter->read<T>());
      }
      DEBUG_LOG("variadic")(vals);
    }

    auto& frame = ctx->call_stack.top();
    frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
                    Fn(std::move(vals)));

    return std::nullopt;
  }

 private:
  static constexpr uint8_t ReverseByte(uint8_t byte) {
    byte = ((byte & 0b11110000) >> 4) | ((byte & 0b00001111) << 4);
    byte = ((byte & 0b11001100) >> 2) | ((byte & 0b00110011) << 2);
    byte = ((byte & 0b10101010) >> 1) | ((byte & 0b01010101) << 1);
    return byte;
  }
};

template <typename CmdType>
RegOr<typename CmdType::type> MakeVariadicImpl(
    absl::Span<RegOr<typename CmdType::type> const> vals) {
  using T = typename CmdType::type;
  {
    std::vector<T> vs;
    vs.reserve(vals.size());
    if (absl::c_all_of(vals, [&](RegOr<T> t) {
          if (t.is_reg_) { return false; }
          vs.push_back(t.val_);
          return true;
        })) {
      return CmdType::fn_ptr(vs);
    }
  }

  auto& blk = GetBlock();
  blk.cmd_buffer_.append_index<CmdType>();
  blk.cmd_buffer_.append<uint16_t>(vals.size());
  uint8_t reg_mask = 0;
  for (size_t i = 0; i < vals.size(); ++i) {
    if (vals[i].is_reg_) { reg_mask |= (1 << (7 - (i % 8))); }
    if (i % 8 == 7) {
      blk.cmd_buffer_.append(reg_mask);
      reg_mask = 0;
    }
  }
  if (vals.size() % 8 != 0) { blk.cmd_buffer_.append(reg_mask); }

  absl::c_for_each(vals, [&](RegOr<T> t) {
    if (t.is_reg_) {
      blk.cmd_buffer_.append(t.reg_);
    } else {
      blk.cmd_buffer_.append(t.val_);
    }
  });

  Reg result = MakeResult(type::Get<T>());
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("variadic")(blk.cmd_buffer_.to_string());
  return RegOr<T>{result};
}

}  // namespace internal
}  // namespace ir

#endif  // ICARUS_IR_CMD_UTIL_H
