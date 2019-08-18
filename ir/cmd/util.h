#ifndef ICARUS_IR_CMD_UTIL_H
#define ICARUS_IR_CMD_UTIL_H

#include <type_traits>

#include "backend/exec.h"
#include "base/util.h"
#include "ir/addr.h"
#include "ir/reg.h"

namespace type {
struct Type;
}  // namespace type

namespace ir {
using cmd_index_t = uint8_t;

template <typename T>
constexpr uint8_t PrimitiveIndex() {
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
  } else if constexpr (std::is_same_v<T, AnyFunc>) {
    return 0x10;
  } else if constexpr (std::is_same_v<T, ast::FunctionLiteral*>) {
    // TODO: FunctionLiteral is a short-term hack for generics. IR shouldn't depend on it.
    return 0x11;
  } else if constexpr (std::is_integral_v<T>) {
    return base::Log2(sizeof(T)) * 2 + std::is_signed_v<T>;
  } else {
    UNREACHABLE(typeid(T).name());
  }
}

template <typename Fn>
auto PrimitiveDispatch(uint8_t primitive_type, Fn&& fn) {
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
    case PrimitiveIndex<AnyFunc>():
      return std::forward<Fn>(fn)(base::Tag<AnyFunc>{});
    case PrimitiveIndex<ast::FunctionLiteral*>():
      return std::forward<Fn>(fn)(base::Tag<ast::FunctionLiteral*>{});
    case PrimitiveIndex<EnumVal>():
      return std::forward<Fn>(fn)(base::Tag<EnumVal>{});
    case PrimitiveIndex<FlagsVal>():
      return std::forward<Fn>(fn)(base::Tag<FlagsVal>{});
    default: UNREACHABLE(static_cast<int>(primitive_type));
  }
}

inline type::Type const* GetType(uint8_t primitive_type) {
  return PrimitiveDispatch(primitive_type, [](auto tag) {
    return type::Get<typename std::decay_t<decltype(tag)>::type>();
  });
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

template <typename SizeType, typename T>
void Serialize(CmdBuffer* buf, absl::Span<RegOr<T> const> span) {
  ASSERT(span.size() < std::numeric_limits<SizeType>::max());
  buf->append<SizeType>(span.size());

  uint8_t reg_mask = 0;
  for (size_t i = 0; i < span.size(); ++i) {
    if (span[i].is_reg_) { reg_mask |= (1 << (7 - (i % 8))); }
    if (i % 8 == 7) {
      buf->append(reg_mask);
      reg_mask = 0;
    }
  }
  if (span.size() % 8 != 0) { buf->append(reg_mask); }

  absl::c_for_each(span, [&](RegOr<T> x) {
    if (x.is_reg_) {
      buf->append(x.reg_);
    } else {
      buf->append(x.val_);
    }
  });
}

constexpr uint8_t ReverseByte(uint8_t byte) {
  byte = ((byte & 0b11110000) >> 4) | ((byte & 0b00001111) << 4);
  byte = ((byte & 0b11001100) >> 2) | ((byte & 0b00110011) << 2);
  byte = ((byte & 0b10101010) >> 1) | ((byte & 0b01010101) << 1);
  return byte;
}

template <typename SizeType, typename T, typename Fn>
auto Deserialize(base::untyped_buffer::iterator* iter, Fn&& fn) {
  SizeType num    = iter->read<SizeType>();
  uint8_t current = 0;

  std::vector<bool> bits;
  bits.reserve(num);
  for (SizeType i = 0; i < num; ++i) {
    if (i % 8 == 0) { current = ReverseByte(iter->read<uint8_t>()); }
    bits.push_back(current & 1);
    current >>= 1;
  }

  if constexpr (std::is_void_v<decltype(
                    std::forward<Fn>(fn)(std::declval<Reg&>()))>) {
    for (bool b : bits) {
      if (b) {
        std::forward<Fn>(fn)(iter->read<Reg>());
      } else {
        iter->read<T>();
      }
    }
    return;
  } else {
    std::vector<T> vals;
    vals.reserve(num);
    for (bool b : bits) {
      vals.push_back(b ? std::forward<Fn>(fn)(iter->read<Reg>())
                       : iter->read<T>());
    }
    return vals;
  }
}

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

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    auto ctrl = iter->read<control_bits>();
    using base::stringify;
    std::string s = " ";
    if (ctrl.reg0) {
      s.append(stringify(iter->read<Reg>()));
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        s.append(stringify(iter->read<typename std::decay_t<decltype(tag)>::type>()));
      });
    }

    s.append(" -> ");
    s.append(stringify(iter->read<Reg>()));
    return s;
  }

  static void UpdateForInlining(base::untyped_buffer::iterator* iter,
                                Inliner const& inliner) {
    auto ctrl = iter->read<control_bits>();
    // TODO: Add core::LayoutRequirements so you can skip forward by the
    // appropriate amount without instantiating so many templates.
    if (ctrl.reg0) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }
    // Result value
    inliner.Inline(&iter->read<Reg>(), GetType(ctrl.primitive_type));
  }

 private:
  template <typename T>
  static auto Apply(base::untyped_buffer::iterator* iter, bool reg0,
                    backend::ExecContext* ctx) {
    if constexpr (IsSupported<T>()) {
      auto val = reg0 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
      DEBUG_LOG("unary")(val);
      return Fn{}(val);
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

    Reg result = MakeResult<T>();
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

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    auto ctrl = iter->read<control_bits>();
    using base::stringify;
    std::string s = " ";
    if (ctrl.reg0) {
      s.append(stringify(iter->read<Reg>()));
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        s.append(stringify(
            iter->read<typename std::decay_t<decltype(tag)>::type>()));
      });
    }

    s.append(" ");
    if (ctrl.reg1) {
      s.append(stringify(iter->read<Reg>()));
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        s.append(stringify(
            iter->read<typename std::decay_t<decltype(tag)>::type>()));
      });
    }

    s.append(" -> ");
    s.append(stringify(iter->read<Reg>()));
    return s;
  }

  static void UpdateForInlining(base::untyped_buffer::iterator* iter,
                                Inliner const& inliner) {
    auto ctrl = iter->read<control_bits>();
    if (ctrl.reg0) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }

    if (ctrl.reg1) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }

    // Result value
    inliner.Inline(&iter->read<Reg>(), GetType(ctrl.primitive_type));
  }

 private:
  template <typename T>
  static auto Apply(base::untyped_buffer::iterator* iter, bool reg0, bool reg1,
                    backend::ExecContext* ctx) {
    if constexpr (IsSupported<T>()) {
      auto lhs = reg0 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
      auto rhs = reg1 ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
      DEBUG_LOG("binary")(lhs, rhs);
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

    Reg result = MakeResult<T>();
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
    std::vector<T> vals = Deserialize<uint16_t, T>(
        iter, [ctx](Reg& reg) { return ctx->resolve<T>(reg); });

    auto& frame = ctx->call_stack.top();
    frame.regs_.set(GetOffset(frame.fn_, iter->read<Reg>()),
                    Fn(std::move(vals)));

    return std::nullopt;
  }

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    return "NOT_YET";
  }

  static void UpdateForInlining(base::untyped_buffer::iterator* iter,
                                Inliner const& inliner) {
    Deserialize<uint16_t, T>(iter,
                             [&inliner](Reg& reg) { inliner.Inline(&reg); });
    // Result value
    inliner.Inline(&iter->read<Reg>(), ::type::Get<T>());
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
  Serialize<uint16_t>(&blk.cmd_buffer_, vals);

  Reg result = MakeResult<T>();
  blk.cmd_buffer_.append(result);
  DEBUG_LOG("variadic")(blk.cmd_buffer_.to_string());
  return RegOr<T>{result};
}

}  // namespace internal
}  // namespace ir

#endif  // ICARUS_IR_CMD_UTIL_H