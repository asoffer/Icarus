#ifndef ICARUS_BASE_UNTYPED_BUFFER_H
#define ICARUS_BASE_UNTYPED_BUFFER_H

#include <cstring>
#include "base/debug.h"

namespace base {
struct untyped_buffer {
  untyped_buffer(size_t starting_capacity)
      : size_(0),
        capacity_(starting_capacity),
        data_(static_cast<char *>(malloc(starting_capacity))) {}

  static untyped_buffer MakeFull(size_t starting_size) {
    untyped_buffer result(starting_size);
    result.size_ = result.capacity_;
    return result;
  }

  untyped_buffer(untyped_buffer &&that)
      : size_(that.size_), capacity_(that.capacity_), data_(that.data_) {
    that.size_     = 0;
    that.capacity_ = 0;
    that.data_     = nullptr;
  }
  ~untyped_buffer() { free(data_); }

  size_t size() const { return size_; }
  bool empty() const { return size_ == 0; }

  template <typename T>
  T get(size_t offset) const {
    static_assert(std::is_trivially_copyable_v<T>);
    ASSERT(offset + sizeof(T) <= size_);
    T result{};
    std::memcpy(&result, data_ + offset, sizeof(T));
    return result;
  }

  void *raw(size_t offset) {
    ASSERT(offset <= size_);
    return data_ + offset;
  }

  void const *raw(size_t offset) const {
    ASSERT(offset <= size_);
    return data_ + offset;
  }

  template <typename T>
  void set(size_t offset, T const &t) {
    static_assert(std::is_trivially_copyable_v<T>);
    ASSERT(offset + sizeof(T) <= size_);
    std::memcpy(data_ + offset, &t, sizeof(T));
  }

  template <typename T>
  void append(T const &t) {
    size_t old_size = size_;

    append_bytes(sizeof(T), alignof(T));
    size_t old_size_with_alignment = ((old_size - 1) | (alignof(T) - 1)) + 1;
    set(old_size_with_alignment, t);
  }

  void write(size_t offset, base::untyped_buffer const &buf) {
    append_bytes(buf.size(), 1);
    std::memcpy(data_ + offset, buf.data_, buf.size_);
  }

  void append_bytes(size_t num, size_t alignment) {
    // TODO combine with Architecture::MoveForwardToAlignment?
    size_t new_size = ((size_ - 1) | (alignment - 1)) + 1 + num;

    if (new_size > capacity_) { reallocate(new_size); }
    size_ = new_size;
  }

  void pad_to(size_t n) {
    size_t new_size = std::max(n, size_);
    if (new_size > capacity_) { reallocate(new_size); }
    size_ = new_size;
  }

#ifdef DBG
  std::string DebugString() const;
#endif

 private:
  void reallocate(size_t num) {
    size_t new_cap = std::max<size_t>(num, capacity_ * 2);
    char *new_data = static_cast<char *>(malloc(new_cap));
    std::memcpy(new_data, data_, size_);
    capacity_ = new_cap;
    free(data_);
    data_ = new_data;
  }

  size_t size_     = 0;
  size_t capacity_ = 0;
  char *data_      = 0;
};
}  // namespace base

#endif  // ICARUS_BASE_UNTYPED_BUFFER_H
