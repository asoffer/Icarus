#ifndef ICARUS_IR_VALUE_FN_H
#define ICARUS_IR_VALUE_FN_H

#include <cstring>
#include <iostream>

#include "ir/value/foreign_fn.h"
#include "ir/value/native_fn.h"

namespace ir {

// `Fn` represents any callable function in the language (either a `NativeFn` or
// a `ForeignFn`).
struct Fn {
 private:
  using underlying_type = uintptr_t;
  static_assert(alignof(NativeFn) <= alignof(underlying_type));
  static_assert(alignof(ForeignFn) <= alignof(underlying_type));
  static_assert(sizeof(NativeFn) <= sizeof(underlying_type));
  static_assert(sizeof(ForeignFn) <= sizeof(underlying_type));

 public:
  enum class Kind { Native, Foreign };

  // TODO remove this constructor.
  Fn(CompiledFn *f) : Fn(NativeFn(f)) {}

  Fn(NativeFn f) {
    std::memcpy(&data_, &f, sizeof(data_));
    ASSERT(kind() == Kind::Native);
  }

  constexpr Kind kind() const {
    return (data_ & 1) ? Kind::Foreign : Kind::Native;
  }

  type::Function const *type() const {
    switch (kind()) {
      case Kind::Native: return native().type();
      case Kind::Foreign: return foreign().type();
    }
  }

  Fn(ForeignFn f) {
    underlying_type data;
    std::memcpy(&data, &f, sizeof(void (*)()));
    constexpr underlying_type high_bit =
        underlying_type{1} << (std::numeric_limits<underlying_type>::digits -
                               1);
    ASSERT((data & high_bit) == 0u);
    data_ = (data << underlying_type{1});
    data_ |= 1;
    ASSERT(kind() == Kind::Foreign);
  }

  NativeFn native() const {
    ASSERT(kind() == Kind::Native);
    NativeFn f;
    std::memcpy(&f, &data_, sizeof(CompiledFn *));
    return f;
  }

  ForeignFn foreign() const {
    ASSERT(kind() == Kind::Foreign);
    return ForeignFn(data_ >> 1);
  }

 private:
  underlying_type data_;
};

inline std::ostream &operator<<(std::ostream &os, Fn f) {
  switch (f.kind()) {
    case Fn::Kind::Native: return os << f.native();
    case Fn::Kind::Foreign: return os << f.foreign();
  }
}

}  // namespace ir

#endif  // ICARUS_IR_VALUE_FN_H
