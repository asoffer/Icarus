#ifndef ICARUS_TYPE_UTIL_H
#define ICARUS_TYPE_UTIL_H
// TODO this file is terribly named.

#include "base/tag.h"
#include "ir/value/addr.h"
#include "ir/value/enum_and_flags.h"
#include "ir/value/reg.h"
#include "ir/value/string.h"
#include "ir/value/value.h"
#include "type/array.h"
#include "type/basic_type.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/variant.h"

namespace module {
struct BasicModule;
}  // namespace module

namespace ast {
struct FunctionLiteral;
}  // namespace ast

namespace ir {
struct BlockDef;
struct FlagsVal;
struct Fn;
struct GenericFn;
struct ScopeDef;
}  // namespace ir

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
  } else if constexpr (std::is_same_v<T, std::string_view> or
                       std::is_same_v<T, ir::String>) {
    return type::ByteView;
  } else if constexpr (std::is_same_v<T, ir::BlockDef const *>) {
    return type::Block;  // Maybe opt-block?
  } else if constexpr (std::is_same_v<T, type::Type const *>) {
    return type::Type_;
  } else if constexpr (std::is_same_v<T, module::BasicModule *> or
                       std::is_same_v<T, module::BasicModule const *>) {
    return type::Module;
  } else if constexpr (std::is_same_v<T, ir::ScopeDef *>) {
    return type::Scope;
  } else if constexpr (std::is_pointer_v<T>) {
    return Ptr(Get<std::decay_t<decltype(*std::declval<T>())>>());
  } else {
    UNREACHABLE(typeid(T).name());
  }
}

template <typename T>
bool Compare(::type::Type const *t) {
  if constexpr (std::is_same_v<T, bool>) {
    return t == ::type::Bool;
  } else if constexpr (std::is_same_v<T, int8_t>) {
    return t == ::type::Int8;
  } else if constexpr (std::is_same_v<T, int16_t>) {
    return t == ::type::Int16;
  } else if constexpr (std::is_same_v<T, int32_t>) {
    return t == ::type::Int32;
  } else if constexpr (std::is_same_v<T, int64_t>) {
    return t == ::type::Int64;
  } else if constexpr (std::is_same_v<T, uint8_t>) {
    return t == ::type::Nat8;
  } else if constexpr (std::is_same_v<T, uint16_t>) {
    return t == ::type::Nat16;
  } else if constexpr (std::is_same_v<T, uint32_t>) {
    return t == ::type::Nat32;
  } else if constexpr (std::is_same_v<T, uint64_t>) {
    return t == ::type::Nat64;
  } else if constexpr (std::is_same_v<T, float>) {
    return t == ::type::Float32;
  } else if constexpr (std::is_same_v<T, double>) {
    return t == ::type::Float64;
  } else if constexpr (std::is_same_v<T, ::type::Type const *>) {
    return t == ::type::Type_;
  } else if constexpr (std::is_same_v<T, ::type::Struct const *>) {
    return t->is<::type::Struct>();
  } else if constexpr (std::is_same_v<T, std::string_view> or
                       std::is_same_v<T, ir::String>) {
    return t == type::ByteView;
  } else if constexpr (std::is_same_v<T, ir::EnumVal>) {
    return t->is<::type::Enum>();
  } else if constexpr (std::is_same_v<T, ir::FlagsVal>) {
    return t->is<::type::Flags>();
  } else if constexpr (std::is_same_v<T, ir::Addr>) {
    return t->is<::type::Pointer>();
  } else if constexpr (std::is_same_v<T, ir::ScopeDef *>) {
    return t == ::type::Scope;
  } else if constexpr (std::is_same_v<T, ::type::Struct const *>) {
    return t->is<::type::Struct>();
  } else if constexpr (std::is_same_v<T, ir::Fn>) {
    return t->is<::type::Function>();
  } else if constexpr (std::is_same_v<T, ::type::Jump>) {
    return t->is<::type::Jump>();
  } else if constexpr (std::is_same_v<T, ir::GenericFn>) {
    return t->is<::type::GenericFunction>();
  } else if constexpr (std::is_same_v<T, module::BasicModule *> or
                       std::is_same_v<T, module::BasicModule const *>) {
    return t == ::type::Module;
  } else if constexpr (std::is_same_v<T, ir::BlockDef const *> or
                       std::is_same_v<T, ir::BlockDef *>) {
    return t == ::type::Block;
  } else if constexpr (std::is_same_v<T, ir::Jump *>) {
    return t->is<type::Jump>();
  } else {
    UNREACHABLE(t->to_string(), " vs ", typeid(T).name());
  }
}

template <typename... Ts, typename Fn>
auto ApplyTypes(Type const *t, Fn &&fn) {
  // TODO base::NoDestroy would be nice here.
  using return_type =
      decltype(std::forward<Fn>(fn)(base::Tag<base::first_t<Ts...>>{}));

  // Create a static array of funtions that may be called depending on which
  // type matches.
  static auto const *kFnToCall =
      new std::array<return_type (*)(Fn &&), sizeof...(Ts)>{
          [](Fn &&f) { return std::forward<Fn>(f)(base::Tag<Ts>{}); }...};

  // Using fold expressions, take the disjunction of `type::Compare<T>(t)` over
  // all T. This will compute this boolean value until it returns true. However,
  // in each folded expression, we actually use the comma operator to first
  // increment `index`, which means that `index` will be incremented the number
  // until `type::Compare<T>(t)` returns true. This means that after the
  // computation, the value of `index` is one more than the array index for the
  // function we want to call.
  size_t index = 0;
  bool found   = ((++index, type::Compare<Ts>(t)) or ...);
  ASSERT(found == true) << *t;

  return (*kFnToCall)[index - 1](std::forward<Fn>(fn));
}

template <typename Fn>
auto Apply(Type const *t, Fn &&fn) {
  return ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                    uint32_t, uint64_t, float, double, type::Type const *,
                    ir::EnumVal, ir::FlagsVal, ir::Addr, ir::String,
                    module::BasicModule *, ir::ScopeDef *, ir::Fn,
                    ir::BlockDef const *, ir::GenericFn>(t,
                                                         std::forward<Fn>(fn));
}

}  // namespace type

#endif  // ICARUS_TYPE_UTIL_H
