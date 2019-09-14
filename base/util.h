#ifndef ICARUS_BASE_UTIL_H
#define ICARUS_BASE_UTIL_H

#include <memory>
#include <type_traits>

#include "base/debug.h"
#include "base/macros.h"

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

template <typename Base>
struct Cast {
  template <typename T>
  bool is() const {
    STATIC_ASSERT_RELATED(Base, T);
    return dynamic_cast<T const *>(base()) != nullptr;
  }

  template <typename T>
  T &as() & {
    STATIC_ASSERT_RELATED(Base, T);
    return const_cast<T &>(
        static_cast<std::remove_reference_t<decltype(*this)> const *>(this)
            ->template as<T const>());
  }

  template <typename T>
  T *if_as() {
    return dynamic_cast<T *>(base());
  }

  template <typename T>
  T const *if_as() const {
    return dynamic_cast<T const *>(base());
  }

  template <typename T>
  T &&as() && {
    STATIC_ASSERT_RELATED(Base, T);

#if defined(ICARUS_DEBUG)
    return std::move(*ASSERT_NOT_NULL(dynamic_cast<T *>(base())));
#else   // defined(ICARUS_DEBUG)
    return std::move(*reinterpret_cast<T *>(this));
#endif  // defined(ICARUS_DEBUG)
  }

  template <typename T>
  T const &as() const {
    STATIC_ASSERT_RELATED(Base, T);

#if defined(ICARUS_DEBUG)
    return *ASSERT_NOT_NULL(dynamic_cast<T const *>(base()));
#else   // defined(ICARUS_DEBUG)
    return *reinterpret_cast<T const *>(this);
#endif  // defined(ICARUS_DEBUG)
  }

 private:
  Base *base() { return static_cast<Base *>(this); }
  Base const *base() const { return static_cast<Base const *>(this); }
};

template <typename... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <typename... Ts>
overloaded(Ts...)->overloaded<Ts...>;

template <typename T>
constexpr bool always_false() {
  return false;
}

constexpr size_t Log2(size_t n) { return n == 1 ? 0 : 1 + Log2(n / 2); }
}  // namespace base
#undef STATIC_ASSERT_RELATED
#endif  // ICARUS_BASE_UTIL_H
