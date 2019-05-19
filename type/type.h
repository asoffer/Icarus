#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <string>

#include "absl/container/flat_hash_set.h"
#include "base/debug.h"
#include "base/util.h"
#include "core/arch.h"
#include "ir/addr.h"
#include "ir/block.h"
#include "ir/register.h"
#include "ir/results.h"
#include "visitor/emit_ir.h"

struct Context;
struct TextSpan;

struct Module;

namespace ast {
struct ScopeLiteral;
struct FunctionLiteral;
}  // namespace ast

namespace ir {
struct FlagsVal;
struct Val;
struct AnyFunc;
}  // namespace ir

#define TYPE_FNS(name)                                                         \
  name() = delete;                                                             \
  virtual ~name() {}                                                           \
  BASIC_METHODS

#define ENDING = 0
#define BASIC_METHODS                                                          \
  virtual void WriteTo(std::string *buf) const ENDING;                         \
  virtual core::Bytes bytes(core::Arch const &arch) const ENDING;              \
  virtual core::Alignment alignment(core::Arch const &arch) const ENDING;      \
  virtual void defining_modules(                                               \
      absl::flat_hash_set<::Module const *> *modules) const ENDING;            \
  virtual bool ReinterpretAs(Type const *t) const ENDING;                      \
  virtual Cmp Comparator() const ENDING

namespace type {

struct Function;
struct Struct;
struct GenericStruct;
struct Pointer;
struct Interface;

// Note: the order of these is meaningful and relied upon!
enum class Cmp : uint8_t { None, Equality, Order };

struct Type : public base::Cast<Type> {
 public:
  Type() {}
  virtual ~Type() {}
  BASIC_METHODS;

#define ICARUS_TYPE_VISITOR(signature, body)                                   \
  virtual signature { UNREACHABLE(); }
#include "visitor/type_visitors.xmacro.h"
#undef ICARUS_TYPE_VISITOR

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
  bool is_big() const;
  virtual bool needs_destroy() const { return false; }
  virtual bool IsDefaultInitializable() const { return true; }
  virtual bool IsCopyable() const { return true; }
  virtual bool IsMovable() const { return true; }
};

struct Enum;
struct Flags;

Type const *Void();

#define PRIMITIVE_MACRO(EnumName, name) extern Type const *EnumName;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
extern Type const *Generic;

template <typename T>
struct TypeHolder {
  using type = T;
};

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
  } else if constexpr (std::is_same_v<T, ast::ScopeLiteral *>) {
    return t == ::type::Scope;
  } else if constexpr (std::is_same_v<T, ::type::Interface const *>) {
    return t == ::type::Intf;
  } else if constexpr (std::is_same_v<T, ::type::Struct const *>) {
    return t->is<::type::Struct>();
  } else if constexpr (std::is_same_v<T, ir::AnyFunc>) {
    return t->is<::type::Function>();
  } else if constexpr (std::is_same_v<T, ast::FunctionLiteral *>) {
    return t == ::type::Generic;
  } else if constexpr (std::is_same_v<T, ::Module *> ||
                       std::is_same_v<T, ::Module const *>) {
    return t == ::type::Module;
  } else if constexpr (std::is_same_v<T, ir::BlockSequence>) {
    return t == ::type::OptBlock || t == ::type::Block || t == ::type::RepBlock;
  } else {
    UNREACHABLE(t->to_string(), " vs ", typeid(T).name());
  }
}

namespace internal {
template <typename T, typename... Ts>
struct ConditionalApplicator {
  template <typename Fn, typename... Args>
  static auto Apply(type::Type const *t, Fn &&fn, Args &&... args) {
    if constexpr (sizeof...(Ts) == 0) {
      ASSERT(::type::Compare<T>(t) == true)
          << DUMP(t->to_string(), typeid(T).name());
      return std::forward<Fn>(fn)(::type::TypeHolder<T>{},
                                  std::forward<Args>(args)...);
    } else {
      if (::type::Compare<T>(t)) {
        return std::forward<Fn>(fn)(::type::TypeHolder<T>{},
                                    std::forward<Args>(args)...);
      } else {
        return ::type::internal::ConditionalApplicator<Ts...>::Apply(
            t, std::forward<Fn>(fn), std::forward<Args>(args)...);
      }
    }
  }
};

}  // namespace internal

template <typename... Ts, typename Fn, typename... Args>
auto ApplyTypes(Type const *t, Fn &&fn, Args &&... args) {
  return ::type::internal::ConditionalApplicator<Ts...>::Apply(
      t,
      [&](auto type_holder, Args &&... as) {
        return std::forward<Fn>(fn)(type_holder, std::forward<Args>(as)...);
      },
      std::forward<Args>(args)...);
}

template <typename Fn, typename... Args>
auto Apply(Type const *t, Fn &&fn, Args &&... args) {
  return ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                    uint32_t, uint64_t, float, double, type::Type const *,
                    ir::EnumVal, ir::FlagsVal, ir::Addr, std::string_view,
                    ::Module *, type::Struct const *, ast::ScopeLiteral *,
                    ir::AnyFunc, ir::BlockSequence, type::Interface const *,
                    ast::FunctionLiteral *>(t, std::forward<Fn>(fn),
                                            std::forward<Args>(args)...);
}

// TODO lay these out adjacent in memory so the tests can be faster.
inline bool IsIntegral(Type const *t) {
  return t == Int8 || t == Int16 || t == Int32 || t == Int64 || t == Nat8 ||
         t == Nat16 || t == Nat32 || t == Nat64 ;
}

inline bool IsFloatingPoint(Type const *t) {
  return  t == Float32 || t == Float64;
}

inline bool IsNumeric(Type const *t) { return IsIntegral(t) || IsFloatingPoint(t); }

}  // namespace type

#undef ENDING
#define ENDING

#define ICARUS_TYPE_VISITOR(signature, body) signature override body

#endif  // ICARUS_TYPE_TYPE_H
