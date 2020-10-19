#ifndef ICARUS_TYPE_TYPED_VALUE_H
#define ICARUS_TYPE_TYPED_VALUE_H

#include <type_traits>
#include <utility>

#include "absl/strings/str_cat.h"
#include "base/meta.h"
#include "base/stringify.h"

namespace type {
struct Type;

namespace internal {
template <typename T>
using type_t =
    std::conditional_t<base::meta<T> == base::meta<void>, Type, T const*>;
}  // namespace internal

template <typename V, typename T = void>
struct Typed {
  using type_t = internal::type_t<T>;

  template <bool B                   = std::is_default_constructible_v<V>,
            std::enable_if_t<B, int> = 0>
  Typed() {}
  Typed(V value, type_t t) : value_(std::move(value)), type_(t) {}

  template <typename U, std::enable_if_t<
                            base::meta<U>.template inherits_from<T>(), int> = 0>
  Typed(V value, U const* t) : value_(std::move(value)), type_(t) {}

  V& get() & { return value_; }
  V const& get() const& { return value_; }
  V&& get() && { return std::move(value_); }
  V const&& get() const&& { return value_; }

  V* operator->() & { return &value_; }
  V const* operator->() const& { return &value_; }

  V& operator*() & { return value_; }
  V const& operator*() const& { return value_; }

  type_t type() const { return type_; }
  void set_type(type_t t) { type_ = t; }

  template <typename H>
  friend H AbslHashValue(H h, Typed<V, T> const& tv) {
    return H::combine(std::move(h), tv.type(), *tv);
  }

  template <
      typename W, typename U,
      typename = std::enable_if_t<
          base::meta<V>.template converts_to<W>() and
          base::meta<type_t>.template converts_to<internal::type_t<U>>() and
          base::meta<Typed<W, U>> != base::meta<Typed<V, T>>>>
  operator Typed<W, U>() const {
    return Typed<W, U>(value_, type_);
  }

  template <typename U>
  Typed<V, U> as_type() const {
    return Typed<V, U>(value_, &type_->template as<U>());
  }

  friend std::string stringify(Typed const& t) {
    using base::stringify;
    ASSERT(Type(t.type()).valid() == true);
    return absl::StrCat(stringify(t.get()), ": ", t.type()->to_string());
  }

 private:
  V value_{};
  type_t type_{};
};

template <typename V, typename T>
Typed(V, T)->Typed<V, T>;

template <
    typename V, typename T,
    std::enable_if_t<
        std::is_same_v<decltype(std::declval<V>() == std::declval<V>()), bool>,
        int> = 0>
bool operator==(Typed<V, T> const& lhs, Typed<V, T> const& rhs) {
  return lhs.type() == rhs.type() and *lhs == *rhs;
}

template <
    typename V, typename T,
    std::enable_if_t<
        std::is_same_v<decltype(std::declval<V>() == std::declval<V>()), bool>,
        int> = 0>
bool operator!=(Typed<V, T> const& lhs, Typed<V, T> const& rhs) {
  return not(lhs == rhs);
}

}  // namespace type
#endif  // ICARUS_TYPE_TYPED_VALUE_H
