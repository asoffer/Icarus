#ifndef ICARUS_BASE_UNTYPED_BUFFER_VIEW_H
#define ICARUS_BASE_UNTYPED_BUFFER_VIEW_H

#include <cstddef>
#include <string>
#include <vector>

#include "absl/strings/str_join.h"
#include "base/debug.h"
#include "base/untyped_buffer.h"

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

  /* implicit */ untyped_buffer_view(untyped_buffer const &buf)
      : data_(buf.raw(0)), size_(buf.size()) {}

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

  // TODO this was copied from untyped_buffer, so we should move it to a shared
  // location.
  std::string to_string() const { return to_string(8, 0); }
  std::string to_string(size_t width, size_t indent) const {
    constexpr char char_lookup[32] = "0123456789abcdef";

    std::vector<std::string> lines;

    size_t num_left = size_;
    while (num_left != 0) {
      size_t row_width = std::min(width, num_left);
      std::string line(3 * row_width - 1 + indent, ' ');
      char *index = &line[indent];
      for (size_t i = 0; i < row_width; ++i) {
        uint8_t num =
            *reinterpret_cast<uint8_t const *>(raw(size_ - num_left + i));
        uint8_t upper = num / 16;
        uint8_t lower = num & 0x0f;
        *index++      = char_lookup[upper];
        *index++      = char_lookup[lower];
        ++index;  // Skip the next space.
      }
      num_left -= std::min(num_left, width);
      lines.push_back(std::move(line));
    }
    return absl::StrJoin(lines, "\n");
  }

 private:
  std::byte const *data_ = nullptr;
  size_t size_           = 0;
};

}  // namespace base

#endif  // ICARUS_BASE_UNTYPED_BUFFER_VIEW_H
