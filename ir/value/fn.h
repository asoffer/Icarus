#ifndef ICARUS_IR_VALUE_FN_H
#define ICARUS_IR_VALUE_FN_H

#include <cstring>
#include <iostream>

#include "base/debug.h"
#include "core/params.h"
#include "ir/value/builtin_fn.h"
#include "ir/value/foreign_fn.h"
#include "ir/value/native_fn.h"
#include "type/function.h"
#include "type/primitive.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"

namespace ir {

// `Fn` represents any callable function in the language (either a `NativeFn` or
// a `ForeignFn`).
struct Fn : base::Extend<Fn, 1>::With<base::AbslHashExtension> {
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

  // TODO: remove this constructor.
  // TODO: Using default construction to avoid some work needed in byte-code
  // reader.
  Fn(CompiledFn *f = nullptr) : Fn(NativeFn(f)) {}

  Fn(NativeFn f) {
    std::memcpy(&data_, &f, sizeof(data_));
    ASSERT(kind() == Kind::Native);
  }

  constexpr Kind kind() const { return static_cast<Kind>(data_ & 3); }

  size_t num_parameters() const {
    switch (kind()) {
      case ir::Fn::Kind::Native: return native()->num_args();
      case ir::Fn::Kind::Builtin:
      case ir::Fn::Kind::Foreign: return type()->params().size();
    }
  }

  type::Function const *type() const {
    switch (kind()) {
      case Kind::Native: return native().type();
      case Kind::Builtin: {
        switch (builtin().which()) {
          case BuiltinFn::Which::Abort: return type::Func({}, {});
          case BuiltinFn::Which::Alignment:
          case BuiltinFn::Which::Bytes:
            return type::Func(
                {core::AnonymousParam(type::QualType::Constant(type::Type_))},
                {type::U64});
          case BuiltinFn::Which::Opaque: return type::Func({}, {type::Type_});
          case BuiltinFn::Which::Slice:
          case BuiltinFn::Which::Foreign:
            // Note: We do not allow passing `foreign` or `slice` around as a
            // function object. It is call-only, which means the generic part
            // can be handled in the type checker. The value here may be stored,
            // but it will never be accessed again.
            //
            // TODO: Why not allow passing it around?
            return nullptr;
          case BuiltinFn::Which::DebugIr: return type::Func({}, {});
        }
        case Kind::Foreign: return foreign().type();
      }
    }
    UNREACHABLE();
  }

  Fn(ForeignFn f) {
    underlying_type data;
    std::memcpy(&data, &f, sizeof(f));
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
    std::memcpy(&data, &f, sizeof(f));
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
  friend base::EnableExtensions;

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
