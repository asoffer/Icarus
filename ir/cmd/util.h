#ifndef ICARUS_CMD_UTIL_H
#define ICARUS_CMD_UTIL_H

#include <type_traits>

#include "base/util.h"

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
  }
}

}  // namespace ir

#endif  // ICARUS_CMD_UTIL_H
