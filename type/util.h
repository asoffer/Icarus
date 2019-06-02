#ifndef ICARUS_TYPE_UTIL_H
#define ICARUS_TYPE_UTIL_H
// TODO this file is terribly named.

#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/interface.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/variant.h"

namespace type {

template <typename T>
constexpr type::Type const *Get() {
  if constexpr (std::is_same_v<T, bool>) {
    return type::Bool;
  } else if constexpr (std::is_same_v<T, int8_t>) {
    return type::Int8;
  } else if constexpr (std::is_same_v<T, int16_t>) {
    return type::Int16;
  } else if constexpr (std::is_same_v<T, int32_t>) {
    return type::Int32;
  } else if constexpr (std::is_same_v<T, int64_t>) {
    return type::Int64;
  } else if constexpr (std::is_same_v<T, uint8_t>) {
    return type::Nat8;
  } else if constexpr (std::is_same_v<T, uint16_t>) {
    return type::Nat16;
  } else if constexpr (std::is_same_v<T, uint32_t>) {
    return type::Nat32;
  } else if constexpr (std::is_same_v<T, uint64_t>) {
    return type::Nat64;
  } else if constexpr (std::is_same_v<T, float>) {
    return type::Float32;
  } else if constexpr (std::is_same_v<T, double>) {
    return type::Float64;
  } else if constexpr (std::is_same_v<T, std::string_view>) {
    return type::ByteView;
  } else if constexpr (std::is_same_v<T, ir::EnumVal>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<T, ir::FlagsVal>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<T, ir::AnyFunc>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<T, ir::Addr>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<T, ir::BlockDef>) {
    return type::Block; // Maybe opt-block?
  } else if constexpr (std::is_same_v<T, type::Type const *>) {
    return type::Type_;
  } else if constexpr (std::is_same_v<T, type::Interface const *>) {
    return type::Intf;
  } else if constexpr (std::is_same_v<T, ::Module *> ||
                       std::is_same_v<T, ::Module const *>) {
    return type::Module;
  } else if constexpr (std::is_same_v<T, ast::FunctionLiteral *>) {
    return type::Generic;
  } else if constexpr (std::is_same_v<T, ir::ScopeDef *>) {
    return type::Scope;
  } else if constexpr (std::is_pointer_v<T>) {
    return Ptr(Get<std::decay_t<decltype(*std::declval<T>())>>());
  } else {
    UNREACHABLE(typeid(T).name());
  }
}

}  // namespace type
#endif  // ICARUS_TYPE_UTIL_H
