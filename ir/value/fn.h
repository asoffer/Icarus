#ifndef ICARUS_IR_VALUE_FN_H
#define ICARUS_IR_VALUE_FN_H

#include <cstring>
#include <iostream>

#include "base/debug.h"
#include "ir/value/builtin_fn.h"
#include "ir/value/foreign_fn.h"
#include "ir/value/native_fn.h"

namespace ir {

// `Fn` represents any callable function in the language (either a `NativeFn` or
// a `ForeignFn`).
struct Fn {
 private:
  using underlying_type = uintptr_t;
  static_assert(alignof(BuiltinFn) <= alignof(underlying_type));
  static_assert(alignof(NativeFn) <= alignof(underlying_type));
  static_assert(alignof(ForeignFn) <= alignof(underlying_type));
  static_assert(sizeof(BuiltinFn) <= sizeof(underlying_type));
  static_assert(sizeof(NativeFn) <= sizeof(underlying_type));
  static_assert(sizeof(ForeignFn) <= sizeof(underlying_type));

 public:
  enum class Kind { Native, Builtin, Foreign };

  // TODO remove this constructor.
  Fn(CompiledFn *f) : Fn(NativeFn(f)) {}

  Fn(NativeFn f) {
    std::memcpy(&data_, &f, sizeof(data_));
    ASSERT(kind() == Kind::Native);
  }

  constexpr Kind kind() const {
    return static_cast<Kind>(data_ & 3);
  }

  type::Function const *type() const {
    switch (kind()) {
      case Kind::Native: return native().type();
      case Kind::Builtin: return builtin().type();
      case Kind::Foreign: return foreign().type();
    }
    UNREACHABLE();
  }

  Fn(ForeignFn f) {
    underlying_type data;
    std::memcpy(&data, &f, sizeof(void (*)()));
    constexpr underlying_type high_bits =
        underlying_type{3} << (std::numeric_limits<underlying_type>::digits -
                               2);
    ASSERT((data & high_bits) == 0u);
    data_ = (data << underlying_type{2});
    data_ |= 2;
    ASSERT(kind() == Kind::Foreign);
  }

  Fn(BuiltinFn f) {
    underlying_type data;
    std::memcpy(&data, &f, sizeof(void (*)()));
    constexpr underlying_type high_bits =
        underlying_type{3} << (std::numeric_limits<underlying_type>::digits -
                               2);
    ASSERT((data & high_bits) == 0u);
    data_ = (data << underlying_type{2});
    data_ |= 1;
    ASSERT(kind() == Kind::Builtin);
  }

  NativeFn native() const {
    ASSERT(kind() == Kind::Native);
    NativeFn f;
    std::memcpy(&f.fn_, &data_, sizeof(CompiledFn *));
    return f;
  }

  ForeignFn foreign() const {
    ASSERT(kind() == Kind::Foreign);
    return ForeignFn(data_ >> 2);
  }

  BuiltinFn builtin() const {
    ASSERT(kind() == Kind::Builtin);
    return BuiltinFn(static_cast<BuiltinFn::Which>(data_ >> 2));
  }

 private:
  underlying_type data_;
};

inline std::ostream &operator<<(std::ostream &os, Fn f) {
  switch (f.kind()) {
    case Fn::Kind::Native: return os << f.native();
    case Fn::Kind::Builtin: return os << f.builtin();
    case Fn::Kind::Foreign: return os << f.foreign();
  }
  UNREACHABLE();
}

}  // namespace ir

#endif  // ICARUS_IR_VALUE_FN_H
