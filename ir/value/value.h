#ifndef ICARUS_IR_VALUE_VALUE_H
#define ICARUS_IR_VALUE_VALUE_H

#include <cstdint>

#include "base/debug.h"
#include "base/meta.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"

namespace ir {
namespace internal_value {

struct VTable {
  size_t (*hash)(void const*);
  bool (*equals)(void const*, void const*);
  std::ostream& (*stream)(std::ostream&, void const*);
  base::MetaValue type;
};

template <typename T>
VTable VTableFor{
    .hash =
        [](void const* p) {
          return absl::Hash<T>()(*reinterpret_cast<T const*>(p));
        },
    .equals =
        [](void const* lhs, void const* rhs) {
          return *reinterpret_cast<T const*>(lhs) ==
                 *reinterpret_cast<T const*>(rhs);
        },
    .stream = [](std::ostream& os, void const* p) -> std::ostream& {
      return os << *reinterpret_cast<T const*>(p);
    },
    .type = base::meta<T>,
};

// clang-format off
template <typename T, size_t Size, size_t Align>
concept HoldableInValue = (sizeof(T) <= Size) and (alignof(T) <= Align)
  and std::is_trivially_copyable_v<T> and std::is_trivially_destructible_v<T>
  and requires(T t) {
    { absl::Hash<T>{}(t) } -> std::same_as<size_t>;
    { t == t } -> std::same_as<bool>;
    { std::declval<std::ostream>() << t } -> std::same_as<std::ostream&>;
};

// clang-format on

}  // namespace internal_value

// A `Value` represents any register or value constant usable in the
// intermediate representation.
struct Value {
 private:
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

 public:
  static constexpr size_t value_size_v = 16;
  static constexpr size_t alignment_v  = 8;

  explicit Value() : Value(Empty{}) {}

  template <internal_value::HoldableInValue<value_size_v, alignment_v> T>
  explicit Value(T const& t) : vptr_(&internal_value::VTableFor<T>) {
    std::memcpy(data_, &t, sizeof(t));
  }

  Value(Value const&) = default;

  template <internal_value::HoldableInValue<value_size_v, alignment_v> T>
  explicit Value(RegOr<T> const& t) {
    if (t.is_reg()) {
      vptr_  = &internal_value::VTableFor<Reg>;
      type() = base::meta<Reg>;
      new (&get_ref<Reg>()) Reg(t.reg());
    } else {
      vptr_        = &internal_value::VTableFor<T>;
      get_ref<T>() = t.value();
      new (&get_ref<T>()) T(t.value());
    }
  }

  bool empty() const { return get_if<Empty>(); }

  constexpr base::MetaValue type() const { return vptr_->type; }

  void const* raw() const { return data_; }

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
    if (type() == base::meta<T>) { return &get_ref<T>(); }
    return nullptr;
  }

  // Returns a pointer to the stored value if it is of the given type, and null
  // otherwise.
  template <typename T>
  T* get_if() {
    return const_cast<T*>(
        static_cast<Value const*>(this)->template get_if<T>());
  }

  template <typename H>
  friend H AbslHashValue(H h, Value const& v) {
    return H::combine(std::move(h), v.vptr_->hash(v.data_));
  }

  template <typename... Ts, typename F>
  void apply(F&& f) const {
    if (((this->template get_if<Ts>()
              ? (std::forward<F>(f)(this->template get<Ts>()), true)
              : false) or
         ...)) {
      return;
    }
    UNREACHABLE(type().name());
  }

 private:
  template <typename T>
  T const& get_ref() const {
    ASSERT(type() == base::meta<T>);
    return *reinterpret_cast<T const*>(data_);
  }

  friend bool operator==(Value const& lhs, Value const& rhs) {
    if (lhs.type() != rhs.type()) { return false; }
    return lhs.vptr_->equals(lhs.data_, rhs.data_);
  }

  friend bool operator!=(Value const& lhs, Value const& rhs) {
    return not(lhs == rhs);
  }

  friend std::ostream& operator<<(std::ostream& os, Value value) {
    return value.vptr_->stream(os, value.data_);
  }

  template <typename T>
  T& get_ref() {
    return const_cast<T&>(static_cast<Value const*>(this)->get_ref<T>());
  }

  alignas(alignment_v) char data_[value_size_v];
  internal_value::VTable const* vptr_;
};

}  // namespace ir

#endif  // ICARUS_IR_VALUE_VALUE_H
