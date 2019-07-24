#ifndef ICARUS_IR_CMD_UTIL_H
#define ICARUS_IR_CMD_UTIL_H

#include <type_traits>

#include "base/util.h"
#include "ir/addr.h"

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
struct UnwrapType<RegisterOr<T>> {
  using type = T;
};

template <typename T>
struct UnwrapType<TypedRegister<T>> {
  using type = T;
};

template <typename T>
using UnwrapTypeT = typename UnwrapType<T>::type;

}  // namespace internal
}  // namespace ir

#endif  // ICARUS_IR_CMD_UTIL_H
