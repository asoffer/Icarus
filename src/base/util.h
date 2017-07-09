#ifndef ICARUS_BASE_UTIL_H
#define ICARUS_BASE_UTIL_H

#include <type_traits>
#include <memory>

#include "debug.h"

template <typename To, typename From> To *ptr_cast(From* ptr) {
#ifdef DEBUG
  auto result = dynamic_cast<To*>(ptr);
  ASSERT(result, "Failed to convert");
  return result;
#else
  return static_cast<To*>(ptr);
#endif
}

namespace base {
template <typename Base> struct Cast {
  template <typename T> bool is() const {
    static_assert(std::is_base_of<Base, T>::value,
                  "Calling is<...> but there is no inheritance relationship. "
                  "Result is vacuously false.");
    return dynamic_cast<const T *>(reinterpret_cast<const Base *>(this)) !=
           nullptr;
  }
};

template <typename T> std::unique_ptr<T> wrap_unique(T *ptr) {
  return std::unique_ptr<T>(ptr);
}
} // namespace base

#endif // ICARUS_BASE_UTIL_H
