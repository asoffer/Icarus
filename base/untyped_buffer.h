#ifndef ICARUS_BASE_UNTYPED_BUFFER_H
#define ICARUS_BASE_UNTYPED_BUFFER_H

#include <cstddef>
#include <cstring>
#include <string>
#include <type_traits>
#include <utility>

#include "base/debug.h"
#include "base/raw_iterator.h"
#include "base/unaligned_ref.h"
#include "base/untyped_buffer_view.h"

namespace base {
constexpr inline std::byte kUnusedByte{0xaa};

struct untyped_buffer {
  using const_iterator = internal::raw_const_iterator;
  using iterator       = internal::raw_iterator;

  untyped_buffer(const_iterator iter, size_t len)
      : data_(iter.raw(), iter.raw() + len) {}

  untyped_buffer(size_t starting_capacity = 0) { reserve(starting_capacity); }

  void reserve(size_t capacity) { data_.reserve(capacity); }
  void resize(size_t n) { data_.resize(n); }

  constexpr iterator begin() { return iterator(&*data_.begin()); }
  constexpr iterator end() { return iterator(&*data_.end()); }
  constexpr const_iterator begin() const {
    return const_iterator(&*data_.begin());
  }
  constexpr const_iterator end() const { return const_iterator(&*data_.end()); }
  constexpr const_iterator cbegin() const {
    return const_iterator(&*data_.begin());
  }
  constexpr const_iterator cend() const {
    return const_iterator(&*data_.end());
  }

  static untyped_buffer MakeFull(size_t starting_size) {
    untyped_buffer result;
#if !defined(ICARUS_DEBUG)
    result.data_.resize(starting_size);
#else   // defined(ICARUS_DEBUG)
    result.data_.resize(starting_size, kUnusedByte);
#endif  // defined(ICARUS_DEBUG)
    return result;
  }

  untyped_buffer(untyped_buffer &&that) noexcept                 = default;
  untyped_buffer(untyped_buffer const &that) noexcept            = default;
  untyped_buffer &operator=(untyped_buffer &&that) noexcept      = default;
  untyped_buffer &operator=(untyped_buffer const &that) noexcept = default;

  operator untyped_buffer_view() const {
    return untyped_buffer_view(data(), size());
  }

  constexpr std::byte const *data() const { return data_.data(); }
  constexpr std::byte *data() { return data_.data(); }
  constexpr size_t size() const { return data_.size(); }
  constexpr bool empty() const { return data_.empty(); }

  template <typename T>
  T get(size_t offset) const {
    static_assert(std::is_trivially_copyable_v<T>);
    ASSERT(offset + sizeof(T) <= size());
    alignas(T) std::byte buf[sizeof(T)];
    std::memcpy(buf, &data_[offset], sizeof(T));
    return *reinterpret_cast<T *>(buf);
  }

  std::byte *raw(size_t offset) {
    ASSERT(offset <= size());
    return data_.data() + offset;
  }

  std::byte const *raw(size_t offset) const {
    ASSERT(offset <= size());
    return data_.data() + offset;
  }

  template <typename T>
  void set(size_t offset, T const &t) {
    static_assert(
        not std::is_same_v<decltype(base::unaligned_ref(std::declval<T>())),
                           T>);
    static_assert(std::is_trivially_copyable_v<T>);
    ASSERT(offset + sizeof(T) <= size());
    std::memcpy(data_.data() + offset, &t, sizeof(T));
  }

  template <typename T>
  size_t append(T const &t) {
    size_t old_size = append_bytes(sizeof(T));
    set(old_size, t);
    return old_size;
  }

  void write(size_t offset, std::byte const *data, size_t len) {
    append_bytes(len);
    std::memcpy(data_.data() + offset, data, len);
  }

  void write(size_t offset, base::untyped_buffer const &buf) {
    write(offset, buf.data_.data(), buf.size());
  }

  // Returns an offset to the newly appended region
  size_t append_bytes(size_t num) {
    size_t old_size = size();
#if !defined(ICARUS_DEBUG)
    data_.resize(size() + num);
#else   // defined(ICARUS_DEBUG)
    data_.resize(size() + num, kUnusedByte);
#endif  // defined(ICARUS_DEBUG)
    return old_size;
  }

  constexpr void clear() { data_.clear(); }

  template <typename T>
  size_t reserve() {
    static_assert(std::is_trivially_copyable_v<T>);
    return append_bytes(sizeof(T));
  }

 private:
  std::vector<std::byte> data_;
};

}  // namespace base

#endif  // ICARUS_BASE_UNTYPED_BUFFER_H
