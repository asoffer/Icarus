#ifndef ICARUS_IR_VAL_H
#define ICARUS_IR_VAL_H

#include <functional>
#include <iostream>
#include <memory>
#include <variant>

#include "base/strong_types.h"
#include "ir/addr.h"
#include "ir/any_func.h"
#include "ir/flags_val.h"
#include "ir/register.h"
#include "type/flags.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/util.h"

struct Module;

namespace type {
struct Enum;
struct Flags;
struct Pointer;
struct Struct;
}  // namespace type

namespace ast {
struct Expression;
struct ScopeLiteral;
struct BlockLiteral;
struct FunctionLiteral;
}  // namespace ast

namespace ir {
std::string_view SaveStringGlobally(std::string const &str);

struct Val {
  const type::Type *type = nullptr;
  std::variant<Register, ir::Addr, bool, float, double, int8_t, int16_t,
               int32_t, int64_t, uint8_t, uint16_t, uint32_t, uint64_t, EnumVal,
               FlagsVal, type::Type const *, type::Struct *, AnyFunc,
               ast::FunctionLiteral *, std::string_view, ast::ScopeLiteral *,
               type::Interface const *, ast::Expression *, BlockIndex, Module *,
               BlockSequence, BuiltinGenericIndex>
      value{false};

  template <typename T>
  RegisterOr<T> reg_or() const {
    if (auto *r = std::get_if<Register>(&value)) {
      return RegisterOr<T>(*r);
    } else {
      return RegisterOr<T>(std::get<T>(value));
    }
  }

  template <typename T,
            typename = std::enable_if_t<!std::is_same_v<std::decay_t<T>, Val>>>
  explicit Val(T &&val) {
    using decayed = std::decay_t<T>;
    if constexpr (::type::IsTyped<decayed>::value) {
      type  = val.type();
      value = val.get();
    } else if constexpr (IsTypedReg<decayed>::value) {
      type = ::type::Get<typename decayed::type>();
      value = static_cast<Register>(val);
    } else if constexpr (std::is_same_v<decayed, std::string_view>) {
      type  = ::type::ByteView;
      value = SaveStringGlobally(std::string(val));
    } else if constexpr (std::is_same_v<decayed, std::string>) {
      type  = ::type::ByteView;
      value = SaveStringGlobally(val);
    } else {
      type  = ::type::Get<std::decay_t<T>>();
      value = std::forward<T>(val);
    }
  }

  explicit Val(std::nullptr_t) : Val(type::NullPtr, ir::Addr{}) {}
  explicit Val(ast::ScopeLiteral *scope_lit);

  static Val Reg(Register r, const type::Type *t) { return Val(t, r); }
  static Val BuiltinGeneric(int32_t n) {
    return Val(type::Generic, BuiltinGenericIndex{n});
  }
  // TODO take an EnumVal.
  static Val Enum(type::Enum const *enum_type, size_t integral_val);
  static Val Func(type::Type const *t, AnyFunc f) { return Val(t, f); }
  static Val Func(
      ast::FunctionLiteral *fn);  // TODO call this a generic funciton
  static Val BasicBlock(BlockIndex bi) { return Val(nullptr, bi); }
  static Val Block(ast::BlockLiteral *b);
  static Val BlockSeq(BlockSequence b);

  static Val None() { return Val(); }

  std::string to_string() const;

  Val()                = default;
  ~Val() noexcept      = default;
  Val(const Val &)     = default;
  Val(Val &&) noexcept = default;
  Val &operator=(const Val &) = default;
  Val &operator=(Val &&) noexcept = default;

 private:
  friend Val ValFrom(RegisterOr<ir::Addr> r, type::Pointer const *ptr_type);

  template <typename T>
  Val(type::Type const *t, T &&val) : type(t), value(std::forward<T>(val)) {}
};

template <typename T>
inline Val ValFrom(RegisterOr<T> r) {
  return r.is_reg_ ? Val::Reg(r.reg_, type::Get<T>()) : Val(r.val_);
}

Val ValFrom(RegisterOr<EnumVal> r, type::Enum const *t);
Val ValFrom(RegisterOr<FlagsVal> r, type::Flags const *t);
Val ValFrom(RegisterOr<ir::Addr> r, type::Pointer const *ptr_type);

inline bool operator==(const Val &lhs, const Val &rhs) {
  return lhs.type == rhs.type && lhs.value == rhs.value;
}
inline bool operator!=(const Val &lhs, const Val &rhs) { return !(lhs == rhs); }
bool operator<(const ::ir::Val &lhs, const ::ir::Val &rhs);

Addr GetString(std::string const &str);

}  // namespace ir

#endif  // ICARUS_IR_VAL_H
