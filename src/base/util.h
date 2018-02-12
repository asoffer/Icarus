#ifndef ICARUS_BASE_UTIL_H
#define ICARUS_BASE_UTIL_H

#include <memory>
#include <type_traits>

#include "debug.h"

namespace base {
template <typename Base> struct Cast {
  template <typename T> bool is() const {
    static_assert(std::is_base_of_v<Base, T>,
                  "Calling is<...> but there is no inheritance relationship. "
                  "Result is vacuously false.");
    return dynamic_cast<const T *>(reinterpret_cast<const Base *>(this)) !=
           nullptr;
  }

  template <typename T> T &as() & {
    static_assert(std::is_base_of_v<Base, T>,
                  "Calling as<...> but there is no inheritance relationship. "
                  "Result is vacuously false.");
    return const_cast<T &>(
        static_cast<const std::remove_reference_t<decltype(*this)> *>(this)
            ->template as<const T>());
  }

  template <typename T> T &&as() && {
    static_assert(std::is_base_of_v<Base, T>,
                  "Calling as<...> but there is no inheritance relationship. "
                  "Result is vacuously false.");
#ifdef DEBUG
    auto *result = dynamic_cast<T *>(reinterpret_cast<Base *>(this));
    ASSERT(result, "Failed to convert");
    return std::move(*result);
#else
    return std::move(*reinterpret_cast<T *>(this));
#endif
  }

  template <typename T> const T &as() const {
    static_assert(std::is_base_of_v<Base, T>,
                  "Calling as<...> but there is no inheritance relationship. "
                  "Result is vacuously false.");
#ifdef DEBUG
    auto *result =
        dynamic_cast<const T *>(reinterpret_cast<const Base *>(this));
    ASSERT(result, "Failed to convert");
    return *result;
#else
    return *reinterpret_cast<const T *>(this);
#endif
  }
};

template <typename T> std::unique_ptr<T> wrap_unique(T *ptr) {
  return std::unique_ptr<T>(ptr);
}

template <typename... Ts> struct overloaded : Ts... {
  using Ts::operator()...;
};
template <typename... Ts> overloaded(Ts...)->overloaded<Ts...>;
} // namespace base
#endif // ICARUS_BASE_UTIL_H
