#ifndef ICARUS_BASE_UNTYPED_BUFFER_VIEW_H
#define ICARUS_BASE_UNTYPED_BUFFER_VIEW_H

#include <cstddef>
#include <type_traits>

#include "base/debug.h"
#include "base/raw_iterator.h"
#include "base/unaligned_ref.h"

namespace base {

struct untyped_buffer_view {
  constexpr explicit untyped_buffer_view() = default;

  explicit untyped_buffer_view(std::byte const *data, size_t size)
      : data_(data), size_(size) {}

  // TODO: With this constructor, the view here doesn't make as much sense. This
  // is just a view for spans of bytes.
  template <typename T>
  explicit untyped_buffer_view(T const *data)
      : untyped_buffer_view(reinterpret_cast<std::byte const *>(data),
                            sizeof(T)) {}

  using const_iterator = internal::raw_const_iterator;
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

  constexpr std::byte const *data() const { return data_; }
  constexpr size_t size() const { return size_; }
  constexpr bool empty() const { return size_ == 0; }

  template <typename T>
  T get(size_t offset) const {
    static_assert(std::is_trivially_copyable_v<T>);
    ASSERT(offset + sizeof(T) <= size_);
    alignas(T) std::byte buffer[sizeof(T)];
    std::memcpy(&buffer, data_ + offset, sizeof(T));
    return *reinterpret_cast<T *>(buffer);
  }

  std::byte const *raw(size_t offset) const {
    ASSERT(offset <= size_);
    return data_ + offset;
  }

  friend std::ostream &operator<<(std::ostream &os, untyped_buffer_view view);

 private:
  std::byte const *data_ = nullptr;
  size_t size_           = 0;
};

inline std::ostream &operator<<(std::ostream &os, untyped_buffer_view view) {
  constexpr char char_lookup[32] = "0123456789abcdef";

  size_t num_left = view.size_;
  while (num_left != 0) {
    size_t row_width           = std::min(size_t{8}, num_left);
    std::string_view separator = "";
    for (size_t i = 0; i < row_width; ++i) {
      uint8_t num = *reinterpret_cast<uint8_t const *>(
          view.raw(view.size_ - num_left + i));
      uint8_t upper = num / 16;
      uint8_t lower = num & 0x0f;
      os << std::exchange(separator, " ") << char_lookup[upper]
         << char_lookup[lower];
    }
    num_left -= std::min(num_left, size_t{8});
    os << '\n';
  }
  return os;
}

}  // namespace base

#endif  // ICARUS_BASE_UNTYPED_BUFFER_VIEW_H
