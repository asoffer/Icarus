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

  untyped_buffer(untyped_buffer &&that)
      : size_(that.size_), capacity_(that.capacity_), data_(that.data_) {
    that.size_     = 0;
    that.capacity_ = 0;
    that.data_     = nullptr;
  }
  ~untyped_buffer() { free(data_); }

  size_t size() const { return size_; }

  template <typename T>
  T get(size_t offset) const {
    static_assert(std::is_trivial_v<T>);
    ASSERT(offset + sizeof(T) <= size_);
    T result;
    std::memcpy(&result, data_ + offset, sizeof(T));
    return result;
  }

  template <typename T>
  void set(size_t offset, const T &t) {
    static_assert(std::is_trivial_v<T>);
    ASSERT(offset + sizeof(T) <= size_);
    std::memcpy(data_ + offset, &t, sizeof(T));
  }

  template <typename T>
  void append(const T &t) {
    size_t old_size = size_;
    append_bytes(sizeof(T));
    set(old_size, t);
  }

  void append_bytes(size_t num) {
    if (size_ + num > capacity_) { reallocate(); }
    size_ += num;
  }

 private:
  void reallocate() {
    char *new_data = static_cast<char *>(malloc(capacity_ * 2));
    std::memcpy(new_data, data_, size_);
    capacity_ *= 2;
    free(data_);
    data_ = new_data;
  }

  size_t size_     = 0;
  size_t capacity_ = 0;
  char *data_      = 0;
};
}  // namespace base

#endif  // ICARUS_BASE_UNTYPED_BUFFER_H
