#include "ir/block_def.h"
#include "ir/scope_def.h"
#include "ir/value/addr.h"
#include "ir/value/enum_and_flags.h"
#include "ir/value/fn.h"
#include "ir/value/generic_fn.h"
#include "ir/value/string.h"
#include "module/module.h"
#include "type/type.h"

namespace ir::internal {

constexpr size_t Log2(size_t n) { return n == 1 ? 0 : 1 + Log2(n / 2); }

template <typename T>
constexpr uint8_t PrimitiveIndex() {
  if constexpr (std::is_integral_v<T> and not std::is_same_v<T, bool>) {
    return Log2(sizeof(T)) * 2 + std::is_signed_v<T>;
  } else if constexpr (std::is_same_v<T, float>) {
    return 0x08;
  } else if constexpr (std::is_same_v<T, double>) {
    return 0x09;
  } else if constexpr (std::is_same_v<T, type::Type const*>) {
    return 0x0a;
  } else if constexpr (std::is_same_v<T, Addr>) {
    return 0x0b;
  } else if constexpr (std::is_same_v<T, EnumVal>) {
    return 0x0c;
  } else if constexpr (std::is_same_v<T, FlagsVal>) {
    return 0x0d;
  } else if constexpr (std::is_same_v<T, bool>) {
    return 0x0e;
  } else if constexpr (std::is_same_v<T, String>) {
    return 0x0f;
  } else if constexpr (std::is_same_v<T, Fn>) {
    return 0x10;
  } else if constexpr (std::is_same_v<T, core::Alignment>) {
    return 0x11;
  } else if constexpr (std::is_same_v<T, core::Bytes>) {
    return 0x12;
  } else if constexpr (std::is_same_v<T, BlockDef*> or
                       std::is_same_v<T, BlockDef const*>) {
    return 0x13;
  } else if constexpr (std::is_same_v<T, ScopeDef*> or
                       std::is_same_v<T, ScopeDef const*>) {
    return 0x14;
  } else if constexpr (std::is_same_v<T, module::BasicModule*> or
                       std::is_same_v<T, module::BasicModule const*>) {
    return 0x15;
  } else if constexpr (std::is_same_v<T, GenericFn>) {
    return 0x16;
  } else {
    UNREACHABLE(typeid(T).name());
  }
}

template <typename T>
std::string_view TypeToString() {
  if constexpr (std::is_same_v<T, bool>) {
    return "bool";
  } else if constexpr (std::is_same_v<T, int8_t>) {
    return "int8";
  } else if constexpr (std::is_same_v<T, int16_t>) {
    return "int16";
  } else if constexpr (std::is_same_v<T, int32_t>) {
    return "int32";
  } else if constexpr (std::is_same_v<T, int64_t>) {
    return "int64";
  } else if constexpr (std::is_same_v<T, uint8_t>) {
    return "nat8";
  } else if constexpr (std::is_same_v<T, uint16_t>) {
    return "nat16";
  } else if constexpr (std::is_same_v<T, uint32_t>) {
    return "nat32";
  } else if constexpr (std::is_same_v<T, uint64_t>) {
    return "nat64";
  } else if constexpr (std::is_same_v<T, float>) {
    return "float32";
  } else if constexpr (std::is_same_v<T, double>) {
    return "float64";
  } else if constexpr (std::is_same_v<T, String>) {
    return "bytes";
  } else if constexpr (std::is_same_v<T, EnumVal>) {
    return "enum";
  } else if constexpr (std::is_same_v<T, FlagsVal>) {
    return "flags";
  } else if constexpr (std::is_same_v<T, type::Type const*>) {
    return "type";
  } else if constexpr (std::is_same_v<T, Addr>) {
    return "addr";
  } else if constexpr (std::is_same_v<T, Fn>) {
    return "fn";
  } else if constexpr (std::is_same_v<T, module::BasicModule*> or
                       std::is_same_v<T, module::BasicModule const*>) {
    return "module";
  } else if constexpr (std::is_same_v<T, ir::ScopeDef*> or
                       std::is_same_v<T, ir::ScopeDef const*>) {
    return "scope";
  } else if constexpr (std::is_same_v<T, ir::BlockDef*> or
                       std::is_same_v<T, ir::BlockDef const*>) {
    return "block";
  } else if constexpr (std::is_same_v<T, ir::GenericFn>) {
    return "generic-fn";
  } else {
    static_assert(base::always_false<T>());
  }
}

}  // namespace ir::internal
