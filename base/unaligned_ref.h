#ifndef ICARUS_BASE_UNALIGNED_REF_H
#define ICARUS_BASE_UNALIGNED_REF_H

#include <cstdint>
#include <cstring>
#include <type_traits>

namespace base {

template <typename T>
struct unaligned_ref {
  static_assert(std::is_trivial_v<T>);
  explicit constexpr unaligned_ref(void const *ptr)
      : ptr_(reinterpret_cast<uintptr_t>(ptr)) {}
  explicit constexpr unaligned_ref(T const &t)
      : ptr_(reinterpret_cast<uintptr_t>(&t)) {}

  operator T() const {
    T t;
    std::memcpy(&t, reinterpret_cast<void const *>(ptr_), sizeof(T));
    return t;
  }

 private:
  uintptr_t ptr_;
};

}  // namespace base

#endif  // ICARUS_BASE_UNALIGNED_REF_H
