#ifndef ICARUS_BASE_RAW_ITERATOR_H
#define ICARUS_BASE_RAW_ITERATOR_H

#include <string>
#include <compare>

#include "base/unaligned_ref.h"

namespace base::internal {

struct raw_const_iterator {
  explicit constexpr raw_const_iterator(std::byte const *ptr) : ptr_(ptr) {}
  constexpr raw_const_iterator() : raw_const_iterator(nullptr) {}

  constexpr void skip(size_t n) { ptr_ += n; }

  template <typename T>
  unaligned_ref<T const> read() {
    auto result = unaligned_ref<T const>::FromPtr(raw());
    skip(sizeof(T));
    return result;
  }

  constexpr std::byte const *raw() const { return ptr_; }

  auto operator<=>(raw_const_iterator const &) const = default;

  std::string read_bytes_as_string(size_t length) {
    std::string result;
    result.resize(length);
    std::memcpy(result.data(), ptr_, length);
    ptr_ += length;
    return result;
  }

 private:
  friend struct raw_iterator;

  std::byte const *ptr_;
};

struct raw_iterator : raw_const_iterator {
  explicit constexpr raw_iterator(std::byte *ptr) : raw_const_iterator(ptr) {}

  template <typename T>
  unaligned_ref<T> read() {
    auto result = unaligned_ref<T>::FromPtr(raw());
    skip(sizeof(T));
    return result;
  }

  constexpr std::byte *raw() { return const_cast<std::byte *>(ptr_); }
};

}  // namespace base::internal

#endif  // ICARUS_BASE_RAW_ITERATOR_H
