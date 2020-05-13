#ifndef ICARUS_IR_VALUE_VALUE_H
#define ICARUS_IR_VALUE_VALUE_H

#include <cstdint>

#include "absl/types/span.h"
#include "base/debug.h"
#include "base/meta.h"
#include "ir/value/addr.h"
#include "ir/value/enum_and_flags.h"
#include "ir/value/label.h"
#include "ir/value/reg_or.h"
#include "ir/value/string.h"
#include "type/type_fwd.h"

namespace ir {
struct GenericFn;
struct Fn;
struct Value;

// A `MultiValue` represents a sequence of `Value`s (defined below) packed in
// such a way that they can be stored inside a single `Value`.
struct MultiValue {
  MultiValue(absl::Span<Value const> values);
  MultiValue(MultiValue const&);
  MultiValue(MultiValue&&) = default;

  MultiValue& operator=(MultiValue const&);
  MultiValue& operator=(MultiValue&&) = default;

  ~MultiValue();

  Value const& operator[](size_t n) const;
  Value& operator[](size_t n);

  size_t size() const;

  absl::Span<Value const> span() const;
  absl::Span<Value> span();

  friend bool operator==(MultiValue const& lhs, MultiValue const& rhs);
  friend bool operator!=(MultiValue const& lhs, MultiValue const& rhs) {
    return not(lhs == rhs);
  }

 private:
  std::unique_ptr<char[]> data_;
};

// A `Value` represents any register or value constant usable in the
// intermediate representation.
struct Value {
  // `Empty` is a special tag to hold empty values.
  struct Empty{};

  explicit Value() : Value(Empty{}) {}

  // Constructs a `Value` from the passed in type. The parameter may be of any
  // type supported by `Value` or an `ir::RegOr<T>` where `T` is an type
  // supported by `Value`.
  template <typename T>
  Value(T val) : type_(base::meta<T>) {
    if constexpr (IsRegOr<T>::value) {
      if (val.is_reg()) {
        type_          = base::meta<Reg>;
        get_ref<Reg>() = val.reg();
      } else {
        type_ = base::meta<typename IsRegOr<T>::type>;
        get_ref<typename IsRegOr<T>::type>() = std::move(val).value();
      }
    } else {
      new (&get_ref<T>()) T(std::move(val));
    }
  }

  bool empty() const { return get_if<Empty>(); }

  Value(Value const& v) : type_(v.type_) {
    if (v.type_ == base::meta<MultiValue>) {
      new (&get_ref<MultiValue>()) MultiValue(v.get_ref<MultiValue>());
    } else {
      std::memcpy(buf_, v.buf_, sizeof(buf_));
    }
  }

  Value(Value&& v) : type_(v.type_) {
    if (v.type_ == base::meta<MultiValue>) {
      new (&get_ref<MultiValue>()) MultiValue(v.get<MultiValue>());
    } else {
      std::memcpy(buf_, v.buf_, sizeof(buf_));
    }
  }

  Value& operator=(Value const& v) {
    if (v.type_ == base::meta<MultiValue>) {
      if (type_ == base::meta<MultiValue>) {
        get_ref<MultiValue>() = v.get<MultiValue>();
      } else {
        type_ = base::meta<MultiValue>;
        new (&get_ref<MultiValue>()) MultiValue(v.get<MultiValue>());
      }
    } else {
      if (type_ == base::meta<MultiValue>) {
        get_ref<MultiValue>().~MultiValue();
      }
      std::memcpy(buf_, v.buf_, sizeof(buf_));
    }
    type_ = v.type_;
    return *this;
  }

  Value& operator=(Value&& v) {
    if (type_ == base::meta<MultiValue>) {
      if (type_ == base::meta<MultiValue>) {
        get_ref<MultiValue>() = std::move(v.get_ref<MultiValue>());
      } else {
        type_ = v.type_;
        new (&get_ref<MultiValue>())
            MultiValue(std::move(v.get_ref<MultiValue>()));
      }
    } else {
      if (type_ == base::meta<MultiValue>) {
        get_ref<MultiValue>().~MultiValue();
      }
      std::memcpy(buf_, v.buf_, sizeof(buf_));
    }
    type_ = v.type_;
    return *this;
  }

  ~Value() {
    if (type_ == base::meta<MultiValue>) {
      get_ref<MultiValue>().~MultiValue();
    }
  }

  // Returns the stored value. Behavior is undefined if the stored type is not
  // the same as the template parameter.
  template <typename T>
  T get() const {
    if constexpr (IsRegOr<T>::value) {
      if (auto const* r = get_if<Reg>()) { return *r; }
      return get<typename IsRegOr<T>::type>();
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
               uint32_t, uint64_t, float, double, type::Type const*, Addr,
               String, EnumVal, FlagsVal, /*Fn, GenericFn,*/ Reg>(
        std::forward<F>(f));
  }

  template <typename H>
  friend H AbslHashValue(H h, Value const& v) {
    v.apply_impl<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                 uint32_t, uint64_t, float, double, type::Type const*, Addr,
                 /*String,*/ EnumVal, FlagsVal, /* Fn, GenericFn, */ Reg>(
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
    static_assert(sizeof(T) <= 8);
    ASSERT(type_ == base::meta<T>);
    return *reinterpret_cast<T const*>(buf_);
  }

  friend bool operator==(Value const& lhs, Value const& rhs) {
    if (lhs.type_ != rhs.type_) { return false; }
    bool eq;
    lhs.apply_impl<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                   uint32_t, uint64_t, float, double, type::Type const*, Reg,
                   MultiValue>([&rhs, &eq](auto x) {
      eq = (x == rhs.get<std::decay_t<decltype(x)>>());
    });
    return eq;
  }

  friend bool operator!=(Value const& lhs, Value const& rhs) {
    return not(lhs == rhs);
  }

  friend std::ostream& operator<<(std::ostream& os, Value value) {
    value.apply_impl<bool, int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                     uint32_t, uint64_t, float, double, type::Type const*>(
        [&os](auto x) { os << x; });
    return os;
  }

  template <typename T>
  T& get_ref() {
    return const_cast<T&>(static_cast<Value const*>(this)->get_ref<T>());
  }

  base::MetaValue type_;
  alignas(8) char buf_[8];
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_VALUE_H
