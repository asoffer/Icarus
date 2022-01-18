#ifndef ICARUS_BASE_UNALIGNED_REF_H
#define ICARUS_BASE_UNALIGNED_REF_H

#include <cstdint>
#include <cstring>
#include <type_traits>

namespace base {

template <typename T>
struct unaligned_ref {
  static_assert(std::is_trivially_copyable_v<T>);

  static unaligned_ref<T> FromPtr(void *ptr) { return unaligned_ref<T>(ptr); }

  constexpr unaligned_ref(T &t) : ptr_(reinterpret_cast<uintptr_t>(&t)) {}

  constexpr unaligned_ref<T> operator=(T const &val) {
    std::memcpy(reinterpret_cast<void *>(ptr_), &val, sizeof(T));
    return *this;
  }

  T get() const {
    alignas(T) char buf[sizeof(T)];
    std::memcpy(buf, reinterpret_cast<void const *>(ptr_), sizeof(T));
    return *reinterpret_cast<T *>(buf);
  }

  operator T() const { return get(); }

 private:
  friend struct unaligned_ref<T const>;

  explicit constexpr unaligned_ref(void *ptr)
      : ptr_(reinterpret_cast<uintptr_t>(ptr)) {}

  uintptr_t ptr_;
};

template <typename T>
struct unaligned_ref<T const> {
  static_assert(std::is_trivially_copyable_v<T>);

  static unaligned_ref<T const> FromPtr(void const *ptr) {
    return unaligned_ref<T const>(ptr);
  }

  constexpr unaligned_ref(unaligned_ref<T> r) : ptr_(r.ptr_) {}
  constexpr unaligned_ref(T &t) : ptr_(reinterpret_cast<uintptr_t>(&t)) {}
  constexpr unaligned_ref(T const &t) : ptr_(reinterpret_cast<uintptr_t>(&t)) {}

  T get() const {
    alignas(T) char buf[sizeof(T)];
    std::memcpy(buf, reinterpret_cast<void const *>(ptr_), sizeof(T));
    return *reinterpret_cast<T *>(buf);
  }

  operator T() const { return get(); }

 private:
  explicit constexpr unaligned_ref(void const *ptr)
      : ptr_(reinterpret_cast<uintptr_t>(ptr)) {}

  uintptr_t ptr_;
};

template <typename T>
unaligned_ref(T const &) -> unaligned_ref<T const>;
template <typename T>
unaligned_ref(T &) -> unaligned_ref<T>;

}  // namespace base

#endif  // ICARUS_BASE_UNALIGNED_REF_H
