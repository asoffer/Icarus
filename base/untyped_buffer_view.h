#ifndef ICARUS_BASE_UNTYPED_BUFFER_VIEW_H
#define ICARUS_BASE_UNTYPED_BUFFER_VIEW_H

#include "base/debug.h"
#include "base/untyped_buffer.h"

namespace base {

struct untyped_buffer_view {
  /* implicit */ untyped_buffer_view(untyped_buffer const &buf)
      : data_(reinterpret_cast<char const *>(buf.raw(0))),
        size_(buf.size()) {}

  using const_iterator = untyped_buffer::const_iterator;
  constexpr const_iterator begin() const { return const_iterator(data_); }
  constexpr const_iterator end() const { return const_iterator(data_ + size_); }
  constexpr const_iterator cbegin() const { return const_iterator(data_); }
  constexpr const_iterator cend() const {
    return const_iterator(data_ + size_);
  }

  void remove_prefix(size_t num) {
    data_ += num;
    size_ -= num;
  }

  void remove_suffix(size_t num) { size_ -= num; }

  constexpr size_t size() const { return size_; }
  constexpr bool empty() const { return size_ == 0; }

  template <typename T>
  T get(size_t offset) const {
    static_assert(std::is_trivially_copyable_v<T>);
    ASSERT(offset + sizeof(T) <= size_);
    T result{};
    std::memcpy(&result, data_ + offset, sizeof(T));
    return result;
  }

  void const *raw(size_t offset) const {
    ASSERT(offset <= size_);
    return data_ + offset;
  }

 private:
  char const *data_ = nullptr;
  size_t size_      = 0;
};

}  // namespace base

#endif  // ICARUS_BASE_UNTYPED_BUFFER_VIEW_H
