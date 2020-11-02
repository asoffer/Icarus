#ifndef ICARUS_IR_VALUE_VALUE_H
#define ICARUS_IR_VALUE_VALUE_H

#include <cstdint>

#include "absl/types/span.h"
#include "base/debug.h"
#include "base/meta.h"
#include "ir/value/addr.h"
#include "ir/value/block.h"
#include "ir/value/enum_and_flags.h"
#include "ir/value/hashtag.h"
#include "ir/value/jump.h"
#include "ir/value/label.h"
#include "ir/value/module_id.h"
#include "ir/value/reg_or.h"
#include "ir/value/scope.h"
#include "ir/value/string.h"
#include "type/type.h"

namespace ir {
// TODO: Invert the dependencies here.
struct Fn;
struct GenericFn;

// A `Value` represents any register or value constant usable in the
// intermediate representation.
struct Value {
  // `Empty` is a special tag to hold empty values.
  struct Empty {
    template <typename H>
    friend H AbslHashValue(H h, Empty) {
      return h;
    }
    friend std::ostream& operator<<(std::ostream& os, Empty) {
      return os << "(empty)";
    }
    friend constexpr bool operator==(Empty, Empty) { return true; }
    friend constexpr bool operator!=(Empty, Empty) { return false; }
  };

  explicit Value() : Value(Empty{}) {}

  using supported_types =
      base::type_list<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                      uint16_t, uint32_t, uint64_t, float, double, type::Type,
                      Reg, Addr, String, FlagsVal, EnumVal, ModuleId, Fn,
                      GenericFn, Jump, Block, Hashtag, Scope, Label, Empty>;

  static constexpr size_t value_size_v = std::max(
      {sizeof(bool),     sizeof(int8_t),   sizeof(int16_t),  sizeof(int32_t),
       sizeof(int64_t),  sizeof(uint8_t),  sizeof(uint16_t), sizeof(uint32_t),
       sizeof(uint64_t), sizeof(float),    sizeof(double),   sizeof(type::Type),
       sizeof(Reg),      sizeof(Addr),     sizeof(String),   sizeof(FlagsVal),
       sizeof(EnumVal),  sizeof(ModuleId), sizeof(Jump),     sizeof(Block),
       sizeof(Hashtag),  sizeof(Scope),    sizeof(Label),    sizeof(Empty)});
  // The above happen to cover the alignment of Fn, GenericFn, but we have
  // layering issues here and those are incomplete when this line is processed.

 private:
  static constexpr size_t alignment_v =
      std::max({alignof(bool),     alignof(int8_t),   alignof(int16_t),
                alignof(int32_t),  alignof(int64_t),  alignof(uint8_t),
                alignof(uint16_t), alignof(uint32_t), alignof(uint64_t),
                alignof(float),    alignof(double),   alignof(type::Type),
                alignof(Reg),      alignof(Addr),     alignof(String),
                alignof(FlagsVal), alignof(EnumVal),  alignof(ModuleId),
                alignof(Jump),     alignof(Block),    alignof(Hashtag),
                alignof(Scope),    alignof(Label),    alignof(Empty)});
  // The above happen to cover the alignment of Fn, GenericFn, but we have
  // layering issues here and those are incomplete when this line is processed.

 public:
  // Constructs a `Value` from the passed in type. The parameter may be of any
  // type supported by `Value` or an `ir::RegOr<T>` where `T` is an type
  // supported by `Value`.
  //
  // TODO: The instantiation taking pointers and modules is annoying due to
  // subclasses.
  template <typename T>
  explicit Value(T val) : type_(base::meta<T>) {
    if constexpr (base::meta<T>.template is_a<ir::RegOr>()) {
      static_assert(base::Contains<supported_types, typename T::type>());
      if (val.is_reg()) {
        type_ = base::meta<Reg>;
        new (&get_ref<Reg>()) Reg(val.reg());
      } else {
        using underlying_type      = typename T::type;
        type_                      = base::meta<underlying_type>;
        get_ref<underlying_type>() = std::move(val).value();
        new (&get_ref<underlying_type>())
            underlying_type(std::move(val).value());
      }
    } else {
      static_assert(base::Contains<supported_types, T>());
      new (&get_ref<T>()) T(std::move(val));
    }
  }

  bool empty() const { return get_if<Empty>(); }

  constexpr base::MetaValue type() const { return type_; }

  // Returns the stored value. Behavior is undefined if the stored type is not
  // the same as the template parameter.
  template <typename T>
  T get() const {
    if constexpr (base::meta<T>.template is_a<ir::RegOr>()) {
      if (auto const* r = get_if<Reg>()) { return *r; }
      return get<typename T::type>();
    } else {
      return get_ref<T>();
    }
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
  constexpr void apply(F&& f) const {
    apply_impl<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
               uint32_t, uint64_t, float, double, type::Type, Addr, String,
               EnumVal, FlagsVal, /*Fn, GenericFn,*/ Reg, ModuleId, Empty>(
        std::forward<F>(f));
  }

  template <typename H>
  friend H AbslHashValue(H h, Value const& v) {
    v.apply_impl<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                 uint32_t, uint64_t, float, double, type::Type, Addr,
                 /*String, */ EnumVal, FlagsVal, /* Fn, GenericFn, */ Hashtag,
                 Jump, Reg, ModuleId, Empty>(
        [&](auto x) { h = H::combine(std::move(h), v.type_.get(), x); });
    return h;
  }

 private:
  template <typename... Ts, typename F>
  void apply_impl(F&& f) const {
    if (((this->template get_if<Ts>()
              ? (std::forward<F>(f)(*this->template get_if<Ts>()), true)
              : false) or
         ...)) {
      return;
    }
    UNREACHABLE(type_.name());
  }

  template <typename T>
  T const& get_ref() const {
    static_assert(base::Contains<supported_types, T>());
    ASSERT(type_ == base::meta<T>);
    return *reinterpret_cast<T const*>(buf_);
  }

  friend bool operator==(Value const& lhs, Value const& rhs) {
    if (lhs.type_ != rhs.type_) { return false; }
    bool eq;
    lhs.apply_impl<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                   uint32_t, uint64_t, float, double, type::Type, Reg, Addr,
                   Hashtag, String, EnumVal, FlagsVal, ModuleId, Empty>(
        [&rhs, &eq](auto x) {
          eq = (x == rhs.get<std::decay_t<decltype(x)>>());
        });
    return eq;
  }

  friend bool operator!=(Value const& lhs, Value const& rhs) {
    return not(lhs == rhs);
  }

  friend std::ostream& operator<<(std::ostream& os, Value value) {
    // TODO: Hack until we invert the Fn dependency.
    if (value.type_ == base::meta<Fn>) { return os << "fn"; }
    value.apply_impl<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                     uint32_t, uint64_t, float, double, type::Type, Reg, Addr,
                     String, FlagsVal, EnumVal, ModuleId, Hashtag, Jump, Block,
                     Scope, Empty>([&os](auto x) { os << x; });
    return os;
  }

  template <typename T>
  T& get_ref() {
    return const_cast<T&>(static_cast<Value const*>(this)->get_ref<T>());
  }

  base::MetaValue type_;
  alignas(alignment_v) char buf_[value_size_v];
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_VALUE_H
