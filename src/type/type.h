#ifndef ICARUS_TYPE_TYPE_H
#define ICARUS_TYPE_TYPE_H

#include <string>

#include "base/debug.h"
#include "base/util.h"
#include "ir/register.h"

struct Context;

#ifdef ICARUS_USE_LLVM
namespace llvm {
class Type;
class LLVMContext;
}  // namespace llvm
#endif  // ICARUS_USE_LLVM

struct Module;

namespace ir {
struct FlagsVal;
struct Val;
}  // namespace ir

#define TYPE_FNS(name)                                                         \
  name() = delete;                                                             \
  virtual ~name() {}                                                           \
  BASIC_METHODS

#define ENDING = 0
#define BASIC_METHODS_WITHOUT_LLVM                                             \
  virtual char *WriteTo(char *buf) const ENDING;                               \
  virtual size_t string_size() const ENDING;                                   \
  virtual void EmitAssign(const Type *from_type, ir::Val from,                 \
                          ir::Register to, Context *ctx) const ENDING;         \
  virtual void EmitInit(ir::Register reg, Context *ctx) const ENDING;          \
  virtual void EmitDestroy(ir::Register reg, Context *ctx) const ENDING;       \
  virtual ir::Val PrepareArgument(const Type *t, const ir::Val &val,           \
                                  Context *ctx) const ENDING;                  \
  virtual void EmitRepr(ir::Val const &id_val, Context *ctx) const ENDING;     \
  virtual Cmp Comparator() const ENDING

#ifdef ICARUS_USE_LLVM
#define BASIC_METHODS                                                          \
  BASIC_METHODS_WITHOUT_LLVM;                                                  \
  virtual llvm::Type *llvm(llvm::LLVMContext &) const ENDING
#else
#define BASIC_METHODS BASIC_METHODS_WITHOUT_LLVM
#endif

namespace type {
// Note: the order of these is meaningful and relied upon!
enum class Cmp : u8 { None, Equality, Order };

struct Type : public base::Cast<Type> {
 public:
  Type() {}
  virtual ~Type() {}
  BASIC_METHODS;

  std::string to_string() const {
    std::string result(string_size(), '\0');
    char *end_buf = WriteTo(result.data());
    ASSERT(static_cast<size_t>(end_buf - result.data()) == result.size());
    return result;
  }

  bool is_big() const;
  virtual bool needs_destroy() const { return false; }
};

const Type *Meet(const Type *lhs, const Type *rhs);
const Type *Join(const Type *lhs, const Type *rhs);
bool CanCastImplicitly(const type::Type *from, const type::Type *to);

void EmitCopyInit(const Type *from_type, const Type *to_type, ir::Val from_val,
                  ir::Register to_var, Context *ctx);
void EmitMoveInit(const Type *from_type, const Type *to_type, ir::Val from_val,
                  ir::Register to_var, Context *ctx);

struct Pointer;
struct CharBuffer;
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
  } else if constexpr (std::is_same_v<T, char>) {
    return type::Char;
  } else if constexpr (std::is_same_v<T, i8>) {
    return type::Int8;
  } else if constexpr (std::is_same_v<T, i16>) {
    return type::Int16;
  } else if constexpr (std::is_same_v<T, i32>) {
    return type::Int32;
  } else if constexpr (std::is_same_v<T, i64>) {
    return type::Int64;
  } else if constexpr (std::is_same_v<T, float>) {
    return type::Float32;
  } else if constexpr (std::is_same_v<T, double>) {
    return type::Float64;
  } else if constexpr (std::is_same_v<T, std::string_view>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<T, ir::EnumVal>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<T, ir::FlagsVal>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<T, ir::Addr>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<T, ir::BlockSequence>) {
    UNREACHABLE();
  } else if constexpr (std::is_same_v<
                           std::decay_t<decltype(*std::declval<T>())>,
                           type::Type>) {
    return type::Type_;
  } else if constexpr (std::is_same_v<
                           std::decay_t<decltype(*std::declval<T>())>,
                           ::Module>) {
    return type::Module;
  } else {
    NOT_YET();
  }
}

template <typename T>
struct TypeHolder {
  using type = T;
};


namespace internal {
template <typename T, typename... Ts>
struct ConditionalApplicator {
  template <typename Fn, typename... Args>
  static auto Apply(type::Type const *t, Fn &&fn, Args &&... args) {
    if constexpr (sizeof...(Ts) == 0) {
      ASSERT(t == ::type::Get<T>());
      return std::forward<Fn>(fn)(::type::TypeHolder<T>{},
                                  std::forward<Args>(args)...);
    } else {
      if (t == ::type::Get<T>()) {
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
  if (t->is<type::Enum>()) {
    return std::forward<Fn>(fn)(TypeHolder<ir::EnumVal>{},
                                std::forward<Args>(args)...);
  } else if (t->is<type::Flags>()) {
    return std::forward<Fn>(fn)(TypeHolder<ir::FlagsVal>{},
                                std::forward<Args>(args)...);
  } else if (t->is<Pointer>()) {
    return std::forward<Fn>(fn)(TypeHolder<ir::Addr>{},
                                std::forward<Args>(args)...);
  } else if (t == Block || t == OptBlock) {
    return std::forward<Fn>(fn)(TypeHolder<ir::BlockSequence>{},
                                std::forward<Args>(args)...);
  } else if (t->is<CharBuffer>()) {
    return std::forward<Fn>(fn)(TypeHolder<std::string_view>{},
                                std::forward<Args>(args)...);
  }

  return ApplyTypes<bool, char, i8, i16, i32, i64, float, double,
                    type::Type const *>(t, std::forward<Fn>(fn),
                                        std::forward<Args>(args)...);
}

}  // namespace type

#undef ENDING
#define ENDING

#endif  // ICARUS_TYPE_TYPE_H
