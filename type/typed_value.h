#ifndef ICARUS_TYPE_TYPED_VALUE_H
#define ICARUS_TYPE_TYPED_VALUE_H

#include <iosfwd>
#include <type_traits>

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

  template <typename... Args>
  std::string to_string(Args&&... args) const {
    return value_->to_string(std::forward<Args>(args)...) + ": " +
           type_->to_string();
  }

  template <typename U, typename = std::enable_if_t<std::is_base_of_v<U, T> &&
                                                    !std::is_same_v<U, T>>>
  operator Typed<V, U>() const {
    return Typed<V, U>(value_, type_);
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
std::ostream& operator<<(std::ostream& os, Typed<V> const& t) {
  using base::stringify;
  return os << stringify(t.get()) << ": " << t.type()->to_string();
}

template <typename T>
struct IsTyped : public std::false_type {};
template <typename T, typename V>
struct IsTyped<Typed<T, V>> : public std::true_type {};
}  // namespace type
#endif  // ICARUS_TYPE_TYPED_VALUE_H
