#ifndef ICARUS_TYPE_TYPED_VALUE_H
#define ICARUS_TYPE_TYPED_VALUE_H

#include <type_traits>
#include <utility>
#include "absl/strings/str_cat.h"
#include "base/stringify.h"

namespace type {
struct Type;

template <typename V, typename T = Type>
struct Typed {
  Typed() = default;
  Typed(V value, T const* t) : value_(std::move(value)), type_(t) {}

  V& get() & { return value_; }
  V const& get() const & { return value_; }
  V&& get() && { return std::move(value_); }
  V const&& get() const && { return value_; }

  V* operator->() & { return &value_; }
  V const* operator->() const & { return &value_; }

  V& operator*() & { return value_; }
  V const& operator*() const & { return value_; }

  T const* type() const { return type_; }
  void set_type(T const* t) { type_ = t; }

  template <typename W, typename U,
            typename = std::enable_if_t<
                std::is_convertible_v<V, W> and std::is_base_of_v<U, T> and
                not std::is_same_v<Typed<W, U>, Typed<V, T>>>>
  operator Typed<W, U>() const {
    return Typed<W, U>(value_, type_);
  }

  template <typename U>
  Typed<V, U> as_type() const {
    return Typed<V, U>(value_, &type_->template as<U>());
  }

 private:
  V value_{};
  T const* type_ = nullptr;
};

template <typename V>
std::string stringify(Typed<V> const& t) {
  ASSERT(t.type() != nullptr);
  return absl::StrCat(stringify(t.get()), ": ", t.type()->to_string());
}

template <typename T>
struct IsTyped : public std::false_type {};
template <typename T, typename V>
struct IsTyped<Typed<T, V>> : public std::true_type {};
}  // namespace type
#endif  // ICARUS_TYPE_TYPED_VALUE_H
