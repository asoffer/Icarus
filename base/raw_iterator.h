#ifndef ICARUS_BASE_RAW_ITERATOR_H
#define ICARUS_BASE_RAW_ITERATOR_H

#include <compare>

#include "base/unaligned_ref.h"

namespace base::internal {

struct raw_const_iterator {
  explicit constexpr raw_const_iterator(char const *ptr) : ptr_(ptr) {}
  constexpr raw_const_iterator() : raw_const_iterator(nullptr) {}

  constexpr void skip(size_t n) { ptr_ += n; }

  template <typename T>
  unaligned_ref<T const> read() {
    auto result = unaligned_ref<T const>::FromPtr(raw());
    skip(sizeof(T));
    return result;
  }

  constexpr void const *raw() const { return ptr_; }

  auto operator<=>(raw_const_iterator const &) const = default;

 private:
  friend struct raw_iterator;

  char const *ptr_;
};

struct raw_iterator : raw_const_iterator {
  explicit constexpr raw_iterator(char *ptr) : raw_const_iterator(ptr) {}

  template <typename T>
  unaligned_ref<T> read() {
    auto result = unaligned_ref<T>::FromPtr(raw());
    skip(sizeof(T));
    return result;
  }

  constexpr void *raw() { return const_cast<char *>(ptr_); }
};

}  // namespace base::internal

#endif  // ICARUS_BASE_RAW_ITERATOR_H
