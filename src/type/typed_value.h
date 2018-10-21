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
      : value_(std::move(value)), type_(&t->as<T>()) {}

  V& get() & { return value_; }
  V const& get() const & { return value_; }
  V&& get() && { return value_; }
  V const&& get() const && { return value_; }

  V* operator->() & { return &value_; }
  V const* operator->() const & { return &value_; }

  V& operator*() & { return value_; }
  V const& operator*() const & { return value_; }

  T const* type() const { return type_; }

  template <typename D>
  operator Typed<D>() const {
    return Typed<D>{static_cast<D const>(value_), type_};
  }

 private:
  V value_;
  T const* type_;
};
}  // namespace type
#endif  // ICARUS_TYPE_TYPED_VALUE_H
