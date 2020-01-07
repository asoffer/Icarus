#ifndef ICARUS_BASE_CLONE_H
#define ICARUS_BASE_CLONE_H

#include <iostream>
#include <memory>
#include <type_traits>
#include <utility>

namespace base {

template <typename Derived, typename Base>
struct Clone : Base {
  Clone(Clone const &) = default;
  template <typename... Args>
  Clone(Args &&... args) : Base(std::forward<Args>(args)...) {}
  Clone &operator=(Clone const &) = default;
  Clone &operator=(Clone &&) noexcept = default;

  std::unique_ptr<Derived> clone() const {
    return std::unique_ptr<Derived>(static_cast<Derived *>(clone_raw()));
  }
  Clone *clone_raw() const override {
    return new Derived(static_cast<Derived const &>(*this));
  }
};

template <typename T>
struct Clone<T, void> {
  std::unique_ptr<T> clone() const {
    return std::unique_ptr<T>(static_cast<T *>(clone_raw()));
  };
  virtual Clone *clone_raw() const { return nullptr; };
};

}  // namespace base

#endif  // ICARUS_BASE_CLONE_H
