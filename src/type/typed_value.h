#ifndef ICARUS_TYPE_TYPED_VALUE_H
#define ICARUS_TYPE_TYPED_VALUE_H

#include <type_traits>

namespace type {
struct Type;

template <typename T>
struct Typed {
  Typed(T value, type::Type const* t) : value_(std::move(value)), type_(t) {}

  T& get() & { return value_; }
  T const& get() const & { return value_; }
  T&& get() && { return value_; }
  T const&& get() const && { return value_; }

  T* operator->() & { return &value_; }
  T const* operator->() const & { return &value_; }

  T& operator*() & { return value_; }
  T const& operator*() const & { return value_; }

  type::Type const* type() const { return type_; }

  template <typename D>
  operator Typed<D>() {
    return Typed<D>{static_cast<D>(value_), type_};
  }

 private:
  T value_;
  type::Type const* type_;
};
}  // namespace type
#endif  // ICARUS_TYPE_TYPED_VALUE_H
