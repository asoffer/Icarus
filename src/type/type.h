#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <string>
#include <unordered_set>

#include "base/debug.h"
#include "base/util.h"
#include "ir/addr.h"
#include "ir/register.h"

struct Context;
struct TextSpan;

#ifdef ICARUS_USE_LLVM
namespace llvm {
class Type;
class LLVMContext;
}  // namespace llvm
#endif  // ICARUS_USE_LLVM

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
#define BASIC_METHODS_WITHOUT_LLVM                                             \
  virtual void WriteTo(std::string *buf) const ENDING;                         \
  virtual void EmitAssign(const Type *from_type, ir::Val const &from,          \
                          ir::RegisterOr<ir::Addr> to, Context *ctx)           \
      const ENDING;                                                            \
  virtual void EmitInit(ir::Register reg, Context *ctx) const ENDING;          \
  virtual void EmitDestroy(ir::Register reg, Context *ctx) const ENDING;       \
  virtual ir::Val PrepareArgument(const Type *t, const ir::Val &val,           \
                                  Context *ctx) const ENDING;                  \
  virtual void EmitRepr(ir::Val const &id_val, Context *ctx) const ENDING;     \
  virtual void defining_modules(std::unordered_set<::Module const *> *modules) \
      const ENDING;                                                            \
  virtual Cmp Comparator() const ENDING

#ifdef ICARUS_USE_LLVM
#define BASIC_METHODS                                                          \
  BASIC_METHODS_WITHOUT_LLVM;                                                  \
  virtual llvm::Type *llvm(llvm::LLVMContext &) const ENDING
#else
#define BASIC_METHODS BASIC_METHODS_WITHOUT_LLVM
#endif

namespace type {
struct Function;
struct Struct;
struct GenericStruct;
struct Interface;

// Note: the order of these is meaningful and relied upon!
enum class Cmp : u8 { None, Equality, Order };

struct Type : public base::Cast<Type> {
 public:
  Type() {}
  virtual ~Type() {}
  BASIC_METHODS;

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
};

void EmitCopyInit(const Type *from_type, const Type *to_type,
                  ir::Val const &from_val, ir::Register to_var, Context *ctx);
void EmitMoveInit(const Type *from_type, const Type *to_type,
                  ir::Val const &from_val, ir::Register to_var, Context *ctx);

struct Pointer;
struct Enum;
struct Flags;

const Type *Void();

#define PRIMITIVE_MACRO(EnumName, name) extern Type const *EnumName;
#include "type/primitive.xmacro.h"
#undef PRIMITIVE_MACRO
extern Type const *Generic;

template <typename T>
constexpr type::Type const *Get() {
  if constexpr (std::is_same_v<T, bool>) {
    return type::Bool;
  } else if constexpr (std::is_same_v<T, i8>) {
    return type::Int8;
  } else if constexpr (std::is_same_v<T, i16>) {
    return type::Int16;
  } else if constexpr (std::is_same_v<T, i32>) {
    return type::Int32;
  } else if constexpr (std::is_same_v<T, i64>) {
    return type::Int64;
  } else if constexpr (std::is_same_v<T, u8>) {
    return type::Nat8;
  } else if constexpr (std::is_same_v<T, u16>) {
    return type::Nat16;
  } else if constexpr (std::is_same_v<T, u32>) {
    return type::Nat32;
  } else if constexpr (std::is_same_v<T, u64>) {
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
  } else if constexpr (std::is_same_v<T, ir::BlockSequence>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<T, type::Type const *>) {
    return type::Type_;
  } else if constexpr (std::is_same_v<T, type::Interface const *>) {
    return type::Intf;
  } else if constexpr (std::is_same_v<T, ::Module const *>) {
    return type::Module;
  } else if constexpr (std::is_same_v<T, ast::FunctionLiteral *>) {
    return type::Generic;
  } else if constexpr (std::is_same_v<T, ast::ScopeLiteral *>) {
    return type::Scope;
  } else {
    UNREACHABLE();
  }
}

template <typename T>
struct TypeHolder {
  using type = T;
};

template <typename T>
bool Compare(::type::Type const *t) {
  if constexpr (std::is_same_v<T, bool>) {
    return t == ::type::Bool;
  } else if constexpr (std::is_same_v<T, i8>) {
    return t == ::type::Int8;
  } else if constexpr (std::is_same_v<T, i16>) {
    return t == ::type::Int16;
  } else if constexpr (std::is_same_v<T, i32>) {
    return t == ::type::Int32;
  } else if constexpr (std::is_same_v<T, i64>) {
    return t == ::type::Int64;
  } else if constexpr (std::is_same_v<T, u8>) {
    return t == ::type::Nat8;
  } else if constexpr (std::is_same_v<T, u16>) {
    return t == ::type::Nat16;
  } else if constexpr (std::is_same_v<T, u32>) {
    return t == ::type::Nat32;
  } else if constexpr (std::is_same_v<T, u64>) {
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
  } else if constexpr (std::is_same_v<T, ::Module const *>) {
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
      ASSERT(::type::Compare<T>(t)) << DUMP(t, typeid(T).name());
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
  return ApplyTypes<bool, i8, i16, i32, i64, u8, u16, u32, u64, float, double,
                    type::Type const *, ir::EnumVal, ir::FlagsVal, ir::Addr,
                    std::string_view, ::Module const *, type::Struct const *,
                    ast::ScopeLiteral *, ir::AnyFunc, ir::BlockSequence,
                    type::Interface const *, ast::FunctionLiteral *>(
      t, std::forward<Fn>(fn), std::forward<Args>(args)...);
}

inline bool IsNumeric(Type const *t) {
  return t == type::Int8 || t == type::Int16 || t == type::Int32 ||
         t == type::Int64 || t == type::Nat8 || t == type::Nat16 ||
         t == type::Nat32 || t == type::Nat64 || t == type::Float32 ||
         t == type::Float64;
}

bool VerifyAssignment(TextSpan const &span, type::Type const *to,
                      type::Type const *from, Context *ctx);

}  // namespace type

#undef ENDING
#define ENDING

#endif  // ICARUS_TYPE_TYPE_H
