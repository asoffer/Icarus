#ifndef ICARUS_BASE_PTR_UNION_H
#define ICARUS_BASE_PTR_UNION_H

#include <algorithm>
#include <iostream>
#include <type_traits>
#include <utility>

#include "base/debug.h"
#include "nth/meta/concepts.h"

namespace base {
namespace internal_ptr_union {

template <typename T, typename... Ts>
concept const_one_of =
    not nth::any_of<T, Ts...> and nth::any_of<T const, Ts const...>;
template <typename T, typename... Ts>
concept const_ptr_one_of = not nth::any_of<T, Ts...> and
                           nth::any_of<std::remove_pointer_t<T> const *, Ts...>;

}  // namespace internal_ptr_union

// Represents a pointer to any one the template parameter types.
template <typename... Ts>
struct PtrUnion {
  static_assert((1 << (std::min({alignof(Ts)...}) - 1)) >= sizeof...(Ts));
  static constexpr size_t alignment_v = std::min({alignof(Ts)...});

  template <nth::any_of<Ts *...> T>
  /* implicit */ PtrUnion(T ptr)
      : ptr_(reinterpret_cast<uintptr_t>(ptr) |
             index<std::remove_pointer_t<T>>()) {}

  template <internal_ptr_union::const_ptr_one_of<Ts *...> T>
  /* implicit */ PtrUnion(T ptr)
      : ptr_(reinterpret_cast<uintptr_t>(ptr) |
             index<std::remove_pointer_t<T>>()) {}

  template <internal_ptr_union::const_one_of<Ts...> T>
  T const *get() const {
    constexpr uintptr_t mask = static_cast<uintptr_t>(alignment_v - 1);
    ASSERT((ptr_ & mask) == index<T>());
    return reinterpret_cast<T const *>(ptr_ & ~mask);
  }

  template <nth::any_of<Ts...> T>
  T *get() {
    constexpr uintptr_t mask = static_cast<uintptr_t>(alignment_v - 1);
    ASSERT((ptr_ & mask) == index<T>());
    return reinterpret_cast<T *>(ptr_ & ~mask);
  }

  template <internal_ptr_union::const_one_of<Ts...> T>
  T const *get_if() const {
    constexpr uintptr_t mask = static_cast<uintptr_t>(alignment_v - 1);
    return (ptr_ & mask) == index<T>()
               ? reinterpret_cast<T const *>(ptr_ & ~mask)
               : nullptr;
  }

  template <nth::any_of<Ts...> T>
  T *get_if() {
    constexpr uintptr_t mask = static_cast<uintptr_t>(alignment_v - 1);
    return (ptr_ & mask) == index<T>() ? reinterpret_cast<T *>(ptr_ & ~mask)
                                       : nullptr;
  }

  template <typename H>
  friend H AbslHashValue(H h, PtrUnion p) {
    return H::combine(std::move(h), p.ptr_);
  }

  friend constexpr bool operator==(PtrUnion lhs, PtrUnion rhs) {
    return lhs.ptr_ == rhs.ptr_;
  }

  friend constexpr bool operator!=(PtrUnion lhs, PtrUnion rhs) {
    return not(lhs == rhs);
  }

 private:
  template <typename T>
  static constexpr uintptr_t index() {
    uintptr_t i      = 0;
    uintptr_t result = 0;
    ((nth::type<T const> == nth::type<Ts const> ? result = i : 0, ++i), ...);
    return result;
  }

  uintptr_t ptr_;
};

}  // namespace base

#endif  // ICARUS_BASE_PTR_UNION_H
