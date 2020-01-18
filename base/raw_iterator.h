#ifndef ICARUS_BASE_RAW_ITERATOR_H
#define ICARUS_BASE_RAW_ITERATOR_H

#include "base/unaligned_ref.h"

namespace base::internal {

struct raw_const_iterator {
  explicit constexpr raw_const_iterator(char const *ptr) : ptr_(ptr) {}

  constexpr void skip(size_t n) { ptr_ += n; }

  template <typename T>
  unaligned_ref<T const> read() {
    auto result = unaligned_ref<T const>::FromPtr(raw());
    skip(sizeof(T));
    return result;
  }

  constexpr void const *raw() const { return ptr_; }

  friend constexpr bool operator<(raw_const_iterator lhs,
                                  raw_const_iterator rhs) {
    return lhs.ptr_ < rhs.ptr_;
  }
  friend constexpr bool operator>(raw_const_iterator lhs,
                                  raw_const_iterator rhs) {
    return rhs < lhs;
  }
  friend constexpr bool operator<=(raw_const_iterator lhs,
                                   raw_const_iterator rhs) {
    return not(lhs > rhs);
  }
  friend constexpr bool operator>=(raw_const_iterator lhs,
                                   raw_const_iterator rhs) {
    return not(rhs > lhs);
  }
  friend constexpr bool operator==(raw_const_iterator lhs,
                                   raw_const_iterator rhs) {
    return lhs.ptr_ == rhs.ptr_;
  }
  friend constexpr bool operator!=(raw_const_iterator lhs,
                                   raw_const_iterator rhs) {
    return not(lhs == rhs);
  }

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
