#ifndef ICARUS_IR_VALUE_VALUE_H
#define ICARUS_IR_VALUE_VALUE_H

#include <cstdint>

#include "base/debug.h"
#include "base/meta.h"
#include "ir/value/addr.h"
#include "ir/value/enum_and_flags.h"
#include "ir/value/fn.h"
#include "ir/value/generic_fn.h"
#include "ir/value/reg.h"
#include "ir/value/string.h"
#include "type/type.h"

namespace ir {

// A `Value` represents any register or value constant usable in the
// intermediate representation.
struct Value {
  // Constructs a `Value` from the passed in type. The parameter may be of any
  // type supported by `Value` or an `ir::RegOr<T>` where `T` is an type
  // supported by `Value`.
  template <typename T>
  Value(T val) : type_(base::meta<T>) {
    if constexpr (IsRegOr<T>::value) {
      if (val.is_reg()) {
        type_ = base::meta<Reg>;
        get_ref<Reg>() = val.reg();
      } else {
        type_ = base::meta<typename IsRegOr<T>::type>;
        get_ref<typename IsRegOr<T>::type>() = val.value();
      }
    } else {
      static_assert(IsSupported<T>());
      get_ref<T>() = val;
    }
  }

  // Returns the stored value. Behavior is undefined if the stored type is not
  // the same as the template parameter.
  template <typename T>
  T get() const {
    return get_ref<T>();
  }


  // Returns a pointer to the stored value if it is of the given type, and null
  // otherwise.
  template <typename T>
  T const* get_if() const {
    if (type_ == base::meta<T>) { return &get_ref<T>(); }
    return nullptr;
  }

  // Returns a pointer to the stored value if it is of the given type, and null
  // otherwise.
  template <typename T>
  T* get_if() {
    return const_cast<T*>(
        static_cast<Value const*>(this)->template get_if<T>());
  }

  // Calls `f` on the held type. `f` must be callable with any type
  // storable by `Value`.
  template <typename F>
  constexpr void apply(F&& f) {
    apply_impl<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
               uint32_t, uint64_t, float, double, type::Type const*, Addr,
               String, EnumVal, FlagsVal, Fn, GenericFn, Reg>(
        std::forward<F>(f));
  }

 private:
  template <typename... Ts, typename F>
  void apply_impl(F&& f) {
    if (((this->template get_if<Ts>()
              ? (std::forward<F>(f)(*this->template get_if<Ts>()), true)
              : false) or
         ...)) {
      return;
    }
    UNREACHABLE(type_.get());
  }

  template <typename T>
  static constexpr bool IsSupported() {
    return (base::meta<T> == base::meta<bool>) or
           (base::meta<T> == base::meta<int8_t>) or
           (base::meta<T> == base::meta<int16_t>) or
           (base::meta<T> == base::meta<int32_t>) or
           (base::meta<T> == base::meta<int64_t>) or
           (base::meta<T> == base::meta<uint8_t>) or
           (base::meta<T> == base::meta<uint16_t>) or
           (base::meta<T> == base::meta<uint32_t>) or
           (base::meta<T> == base::meta<uint64_t>) or
           (base::meta<T> == base::meta<float>) or
           (base::meta<T> == base::meta<double>) or
           (base::meta<T> == base::meta<type::Type const*>) or
           (base::meta<T> == base::meta<Addr>) or
           (base::meta<T> == base::meta<String>) or
           (base::meta<T> == base::meta<EnumVal>) or
           (base::meta<T> == base::meta<FlagsVal>) or
           (base::meta<T> == base::meta<Fn>) or
           (base::meta<T> == base::meta<GenericFn>) or
           (base::meta<T> == base::meta<Reg>);
  }

  template <typename T>
  T const& get_ref() const {
    ASSERT(type_ == base::meta<T>);
    if constexpr (base::meta<T> == base::meta<bool>) {
      return b_;
    } else if constexpr (std::is_integral_v<T>) {
      if constexpr (std::is_signed_v<T>) {
        if constexpr (sizeof(T) == 1) { return i8_; }
        if constexpr (sizeof(T) == 2) { return i16_; }
        if constexpr (sizeof(T) == 4) { return i32_; }
        if constexpr (sizeof(T) == 8) { return i64_; }
      } else {
        if constexpr (sizeof(T) == 1) { return u8_; }
        if constexpr (sizeof(T) == 2) { return u16_; }
        if constexpr (sizeof(T) == 4) { return u32_; }
        if constexpr (sizeof(T) == 8) { return u64_; }
      }
    } else if constexpr (base::meta<T> == base::meta<float>) {
      return f32_;
    } else if constexpr (base::meta<T> == base::meta<double>) {
      return f64_;
    } else if constexpr (base::meta<T> == base::meta<type::Type const*>) {
      return t_;
    } else if constexpr (base::meta<T> == base::meta<Addr>) {
      return addr_;
    } else if constexpr (base::meta<T> == base::meta<String>) {
      return string_;
    } else if constexpr (base::meta<T> == base::meta<EnumVal>) {
      return enum_;
    } else if constexpr (base::meta<T> == base::meta<FlagsVal>) {
      return flags_;
    } else if constexpr (base::meta<T> == base::meta<Fn>) {
      return fn_;
    } else if constexpr (base::meta<T> == base::meta<GenericFn>) {
      return gen_fn_;
    } else if constexpr (base::meta<T> == base::meta<Reg>) {
      return reg_;
    } else {
      static_assert(base::always_false<T>());
    }
  }

  friend std::ostream& operator<<(std::ostream& os, Value value) {
    value.apply([&](auto val) { os << val; });
    return os;
  }

  template <typename T>
  T& get_ref() {
    return const_cast<T&>(static_cast<Value const*>(this)->get_ref<T>());
  }

  base::MetaValue type_;
  union {
    bool b_;
    int8_t i8_;
    int16_t i16_;
    int32_t i32_;
    int64_t i64_;
    uint8_t u8_;
    uint16_t u16_;
    uint32_t u32_;
    uint64_t u64_;
    float f32_;
    double f64_;
    type::Type const* t_;
    Addr addr_;
    String string_;
    EnumVal enum_;
    FlagsVal flags_;
    Fn fn_;
    // Note: The `Fn` type erases more specific function types (foreign,
    // builtin, native) but is intentionally distinct from `GenericFn`.
    GenericFn gen_fn_;
    Reg reg_;
  };
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_VALUE_H
