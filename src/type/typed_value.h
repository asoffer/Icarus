#ifndef ICARUS_TYPE_TYPED_VALUE_H
#define ICARUS_TYPE_TYPED_VALUE_H

#include <type_traits>
#include "type/type.h"

namespace type {
template <typename V, typename T = Type>
struct Typed {
  // TODO: This does a weird thing where it upcasts and then downcasts. Probably
  // free but worth making sure or fixing it.
  Typed(V value, type::Type const* t)
      : value_(std::move(value)), type_(t ? &t->as<T>() : nullptr) {}

  V& get() & { return value_; }
  V const& get() const & { return value_; }
  V&& get() && { return value_; }
  V const&& get() const && { return value_; }

  V* operator->() & { return &value_; }
  V const* operator->() const & { return &value_; }

  V& operator*() & { return value_; }
  V const& operator*() const & { return value_; }

  T const* type() const { return type_; }
  void set_type(T const* t) { type_ = t; }

  template <typename... Args>
  std::string to_string(Args&&... args) const {
    return value_->to_string(std::forward<Args>(args)...) + ": " +
           type_->to_string();
  }

  template <typename D, typename U,
            typename = std::enable_if_t<std::is_base_of_v<U, T> &&
                                        !(std::is_same_v<U, T> &&
                                          std::is_same_v<D, V>)>>
  operator Typed<D, U>() const {
    return Typed<D, U>(value_, type_);
  }

 private:
  V value_;
  T const* type_;
};
}  // namespace type
#endif  // ICARUS_TYPE_TYPED_VALUE_H
