#ifndef ICARUS_BASE_UTIL_H
#define ICARUS_BASE_UTIL_H

#include <memory>
#include <type_traits>

#include "debug.h"

#define STATIC_ASSERT_RELATED(base, derived)                                   \
  static_assert(                                                               \
      std::is_base_of_v<std::remove_cv_t<base>, std::remove_cv_t<derived>> &&  \
          std::is_convertible_v<std::remove_cv_t<derived> *,                   \
                                std::remove_cv_t<base> *>,                     \
      "Calling is<...> but there is no public inheritance relationship. "      \
      "Result is vacuously false.")

namespace base {
template <typename Fn>
struct defer {
  defer(Fn &&fn) : fn_(std::move(fn)) {}
  ~defer() { fn_(); }

 private:
  Fn fn_;
};
template <typename Fn>
defer(Fn &&)->defer<Fn>;

template <typename Base> struct Cast {
  template <typename T> bool is() const {
    STATIC_ASSERT_RELATED(Base, T);
    return dynamic_cast<const T *>(reinterpret_cast<const Base *>(this)) !=
           nullptr;
  }

  template <typename T> T &as() & {
    STATIC_ASSERT_RELATED(Base, T);
    return const_cast<T &>(
        static_cast<const std::remove_reference_t<decltype(*this)> *>(this)
            ->template as<const T>());
  }

  template <typename T> T &&as() && {
    STATIC_ASSERT_RELATED(Base, T);

#ifdef DBG
    return std::move(
        *ASSERT_NOT_NULL(dynamic_cast<T *>(reinterpret_cast<Base *>(this))));
#else
    return std::move(*reinterpret_cast<T *>(this));
#endif
  }

  template <typename T> const T &as() const {
    STATIC_ASSERT_RELATED(Base, T);

#ifdef DBG
    return *ASSERT_NOT_NULL(
        dynamic_cast<const T *>(reinterpret_cast<const Base *>(this)));
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

template <typename Arg>
void hash_combine(size_t &seed, const Arg &arg) {
  seed ^= std::hash<Arg>{}(arg) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

} // namespace base
#undef STATIC_ASSERT_RELATED
#endif // ICARUS_BASE_UTIL_H
