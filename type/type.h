#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <string>

#include "absl/container/flat_hash_set.h"
#include "base/cast.h"
#include "base/debug.h"
#include "base/meta.h"
#include "base/tag.h"
#include "core/arch.h"
#include "ir/value/addr.h"
#include "ir/results.h"
#include "ir/value/reg.h"
#include "ir/value/enum_and_flags.h"
#include "type/basic_type.h"
#include "type/visitor_base.h"

namespace module {
struct BasicModule;
}  // namespace module

namespace ast {
struct FunctionLiteral;
}  // namespace ast

namespace ir {
struct AnyFunc;
struct BlockDef;
struct FlagsVal;
struct ScopeDef;
}  // namespace ir

#define TYPE_FNS(name)                                                         \
  name() = delete;                                                             \
  ~name() {}                                                                   \
  void WriteTo(std::string *buf) const override;                               \
  core::Bytes bytes(core::Arch const &arch) const override;                    \
  core::Alignment alignment(core::Arch const &arch) const override

namespace type {

struct Jump;
struct Function;
struct Struct;
struct GenericStruct;
struct Pointer;

struct Type : public base::Cast<Type> {
 public:
  Type() {}
  virtual ~Type() {}
  virtual void WriteTo(std::string *buf) const                    = 0;
  virtual core::Bytes bytes(core::Arch const &arch) const         = 0;
  virtual core::Alignment alignment(core::Arch const &arch) const = 0;
  bool IsDefaultInitializable() const { return true; }
  bool IsCopyable() const { return true; }
  bool IsMovable() const { return true; }
  bool HasDestructor() const { return false; }

  virtual void Accept(VisitorBase *visitor, void *ret,
                      void *arg_tuple) const = 0;

  // TODO rename so it doesn't have "Test" in the name.
  virtual bool TestEquality(void const *lhs, void const *rhs) const {
    // TODO
    return true;
  }

  std::string to_string() const {
    std::string result;
    WriteTo(&result);
    return result;
  }

  // TODO length-0 arrays and length-1 arrays of small types should be
  // considered small too. Similarly with simple variants and tuples.
  // // TODO make this pure virtual
  virtual bool is_big() const { return true; }
};

struct Enum;
struct Flags;

Type const *Void();

#define PRIMITIVE_MACRO(EnumName, name) extern Type const *EnumName;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
extern Type const *Generic;

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
  } else if constexpr (std::is_same_v<T, std::string_view>) {
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
  } else if constexpr (std::is_same_v<T, ir::AnyFunc>) {
    return t->is<::type::Function>();
  } else if constexpr (std::is_same_v<T, ::type::Jump>) {
    return t->is<::type::Jump>();
  } else if constexpr (std::is_same_v<T, ast::FunctionLiteral *>) {
    return t == ::type::Generic;
  } else if constexpr (std::is_same_v<T, module::BasicModule *> or
                       std::is_same_v<T, module::BasicModule const *>) {
    return t == ::type::Module;
  } else if constexpr (std::is_same_v<T, ir::BlockDef const *>) {
    return t == ::type::Block;
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
  ASSERT(found == true);

  return (*kFnToCall)[index - 1](std::forward<Fn>(fn));
}

template <typename Fn>
auto Apply(Type const *t, Fn &&fn) {
  return ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                    uint32_t, uint64_t, float, double, type::Type const *,
                    ir::EnumVal, ir::FlagsVal, ir::Addr, std::string_view,
                    module::BasicModule *, ir::ScopeDef *, ir::AnyFunc,
                    ir::BlockDef const *>(t, std::forward<Fn>(fn));
}

// TODO lay these out adjacent in memory so the tests can be faster.
inline bool IsIntegral(Type const *t) {
  return t == Int8 or t == Int16 or t == Int32 or t == Int64 or t == Nat8 or
         t == Nat16 or t == Nat32 or t == Nat64;
}

inline bool IsFloatingPoint(Type const *t) {
  return t == Float32 or t == Float64;
}

inline bool IsNumeric(Type const *t) {
  return IsIntegral(t) or IsFloatingPoint(t);
}

}  // namespace type

#endif  // ICARUS_TYPE_TYPE_H
