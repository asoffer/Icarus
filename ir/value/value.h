#ifndef ICARUS_IR_VALUE_VALUE_H
#define ICARUS_IR_VALUE_VALUE_H

#include <cstdint>

#include "base/debug.h"
#include "base/meta.h"
#include "ir/value/addr.h"
#include "ir/value/enum_and_flags.h"
#include "ir/value/fn.h"
#include "ir/value/reg.h"
#include "ir/value/string.h"
#include "type/type.h"

namespace ir {

// A `Value` represents any register or value constant usable in the
// intermediate representation.
struct Value {
  template <typename T>
  Value(T val) : type_(base::meta<T>) {
    get_ref<T>() = val;
  }

  template <typename T>
  T get() const {
    return get_ref<T>();
  }

 private:
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

    } else if constexpr (base::meta<T> == base::meta<Reg>) {
      return reg_;
    } else {
      static_assert(base::always_false<T>());
    }
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
    Reg reg_;
  };
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_VALUE_H
