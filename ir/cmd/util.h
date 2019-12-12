#ifndef ICARUS_IR_CMD_UTIL_H
#define ICARUS_IR_CMD_UTIL_H

#include <type_traits>

#include "base/util.h"
#include "ir/addr.h"
#include "ir/reg.h"
#include "ir/reg_or.h"
#include "ir/values.h"
#include "type/primitive.h"
#include "type/util.h"

namespace ast {
struct FunctionLiteral;
}  // namespace ast

namespace type {
struct Type;
}  // namespace type

namespace ir {
using cmd_index_t = uint8_t;

template <typename T>
constexpr uint8_t PrimitiveIndex() {
  if constexpr (std::is_integral_v<T> and not std::is_same_v<T, bool>) {
    return base::Log2(sizeof(T)) * 2 + std::is_signed_v<T>;
  } else if constexpr (std::is_same_v<T, bool>) {
    return 0x08;
  } else if constexpr (std::is_same_v<T, float>) {
    return 0x09;
  } else if constexpr (std::is_same_v<T, double>) {
    return 0x0a;
  } else if constexpr (std::is_same_v<T, std::string_view>) {
    return 0x0b;
  } else if constexpr (std::is_same_v<T, type::Type const*>) {
    return 0x0c;
  } else if constexpr (std::is_same_v<T, Addr>) {
    return 0x0d;
  } else if constexpr (std::is_same_v<T, EnumVal>) {
    return 0x0e;
  } else if constexpr (std::is_same_v<T, FlagsVal>) {
    return 0x0f;
  } else if constexpr (std::is_same_v<T, AnyFunc>) {
    return 0x10;
  } else if constexpr (std::is_same_v<T, core::Alignment>) {
    return 0x11;
  } else if constexpr (std::is_same_v<T, core::Bytes>) {
    return 0x12;
  } else if constexpr (std::is_same_v<T, ast::FunctionLiteral*> or
                       std::is_same_v<T, ast::FunctionLiteral const*>) {
    // TODO: FunctionLiteral is a short-term hack for generics. IR shouldn't
    // depend on it.
    return 0x13;
  } else if constexpr (std::is_same_v<T, BlockDef*> or
                       std::is_same_v<T, BlockDef const*>) {
    return 0x14;
  } else if constexpr (std::is_same_v<T, ScopeDef*> or
                       std::is_same_v<T, ScopeDef const*>) {
    return 0x15;
  } else if constexpr (std::is_same_v<T, module::BasicModule*> or
                       std::is_same_v<T, module::BasicModule const*>) {
    return 0x16;
  } else {
    UNREACHABLE(typeid(T).name());
  }
}

inline uint8_t PrimitiveIndex(type::Type const* t) {
  if (t == type::Nat8) { return 0x00; }
  if (t == type::Int8) { return 0x01; }
  if (t == type::Nat16) { return 0x02; }
  if (t == type::Int16) { return 0x03; }
  if (t == type::Nat32) { return 0x04; }
  if (t == type::Int32) { return 0x05; }
  if (t == type::Nat64) { return 0x06; }
  if (t == type::Int64) { return 0x07; }
  if (t == type::Bool) { return 0x08; }
  if (t == type::Float32) { return 0x09; }
  if (t == type::Float64) { return 0x0a; }
  if (t == type::ByteView) { return 0x0b; }
  if (t == type::Type_) { return 0x0c; }
  if (t->is<type::Pointer>()) { return 0x0d; }
  if (t->is<type::Enum>()) { return 0x0e; }
  if (t->is<type::Flags>()) { return 0x0f; }
  if (t->is<type::Function>()) { return 0x10; }
  UNREACHABLE(t->to_string());
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
    case PrimitiveIndex<type::Type const*>():
      return std::forward<Fn>(fn)(base::Tag<type::Type const*>{});
    case PrimitiveIndex<Addr>(): return std::forward<Fn>(fn)(base::Tag<Addr>{});
    case PrimitiveIndex<AnyFunc>():
      return std::forward<Fn>(fn)(base::Tag<AnyFunc>{});
    case PrimitiveIndex<core::Alignment>():
      return std::forward<Fn>(fn)(base::Tag<core::Alignment>{});
    case PrimitiveIndex<core::Bytes>():
      return std::forward<Fn>(fn)(base::Tag<core::Bytes>{});
    case PrimitiveIndex<ast::FunctionLiteral*>():
      return std::forward<Fn>(fn)(base::Tag<ast::FunctionLiteral*>{});
    case PrimitiveIndex<EnumVal>():
      return std::forward<Fn>(fn)(base::Tag<EnumVal>{});
    case PrimitiveIndex<FlagsVal>():
      return std::forward<Fn>(fn)(base::Tag<FlagsVal>{});
    case PrimitiveIndex<BlockDef*>():
      return std::forward<Fn>(fn)(base::Tag<BlockDef*>{});
    case PrimitiveIndex<ScopeDef*>():
      return std::forward<Fn>(fn)(base::Tag<ScopeDef*>{});
    case PrimitiveIndex<module::BasicModule*>():
      return std::forward<Fn>(fn)(base::Tag<module::BasicModule*>{});
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
struct UnwrapType<base::Tagged<T, Reg>> {
  using type = T;
};

template <typename T>
using UnwrapTypeT = typename UnwrapType<T>::type;

constexpr uint8_t ReverseByte(uint8_t byte) {
  byte = ((byte & 0b11110000) >> 4) | ((byte & 0b00001111) << 4);
  byte = ((byte & 0b11001100) >> 2) | ((byte & 0b00110011) << 2);
  byte = ((byte & 0b10101010) >> 1) | ((byte & 0b01010101) << 1);
  return byte;
}

template <typename SizeType, typename Iter>
std::vector<bool> ReadBits(Iter* iter) {
  static_assert(std::disjunction_v<
                std::is_same<Iter, base::untyped_buffer::iterator>,
                std::is_same<Iter, base::untyped_buffer::const_iterator>>);
  SizeType num = iter->template read<SizeType>();

  uint8_t current = 0;

  std::vector<bool> bits;
  bits.reserve(num);
  for (SizeType i = 0; i < num; ++i) {
    if (i % 8 == 0) { current = ReverseByte(iter->template read<uint8_t>()); }
    bits.push_back(current & 1);
    current >>= 1;
  }
  return bits;
}

template <typename SizeType, typename T, typename Iter, typename Fn>
auto Deserialize(Iter* iter, Fn&& fn) {
  static_assert(std::disjunction_v<
                std::is_same<Iter, base::untyped_buffer::iterator>,
                std::is_same<Iter, base::untyped_buffer::const_iterator>>);
  auto bits = ReadBits<SizeType>(iter);

  using result_type =
      std::decay_t<decltype(fn(std::declval<base::unaligned_ref<Reg>>()))>;
  if constexpr (std::is_void_v<result_type>) {
    for (bool b : bits) {
      if (b) {
        fn(iter->template read<Reg>());
      } else {
        iter->template read<T>();
      }
    }
    return;
  } else {
    std::vector<result_type> vals;
    vals.reserve(bits.size());
    for (bool b : bits) {
      vals.push_back(b ? fn(iter->template read<Reg>())
                       : static_cast<T>(iter->template read<T>()));
    }
    return vals;
  }
}

template <uint8_t Index, typename Fn, typename... SupportedTypes>
struct UnaryCmd {
  using fn_type                  = Fn;
  constexpr static uint8_t index = Index;

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

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    control_bits ctrl = iter->read<control_bits>();
    using base::stringify;
    std::string s;
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

    s.append(" -> ");
    s.append(stringify(iter->read<Reg>()));
    return s;
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

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    control_bits ctrl = iter->read<control_bits>();
    using base::stringify;
    std::string s;
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
};

template <cmd_index_t Index, typename T, T (*Fn)(std::vector<T>)>
struct VariadicCmd {
  constexpr static cmd_index_t index = Index;
  using type                         = T;
  constexpr static auto* fn_ptr      = Fn;

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    return "NOT_YET";
  }
};

}  // namespace internal
}  // namespace ir

#endif  // ICARUS_IR_CMD_UTIL_H
