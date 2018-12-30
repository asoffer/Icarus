#ifndef ICARUS_BASE_UTIL_H
#define ICARUS_BASE_UTIL_H

#include <memory>
#include <type_traits>

#include "debug.h"

#define CAT(x, y, z) CAT_(x, y, z)
#define CAT_(x, y, z) x##y##z

#define ASSIGN_OR(action, var, expr)                                           \
  ASSIGN_OR_(action, var, expr, CAT(expr__, __LINE__, __))
#define ASSIGN_OR_(action, var, expr, tmp) ASSIGN_OR__(action, var, expr, tmp)

#define ASSIGN_OR__(action, var, expr, temp)                                   \
  auto &&temp = (expr);                                                        \
  if (!temp) {                                                                 \
    auto &&_ = std::move(temp);                                                \
    action;                                                                    \
  }                                                                            \
  var = std::move(temp)

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

#ifdef DBG
    return std::move(*ASSERT_NOT_NULL(dynamic_cast<T *>(base())));
#else
    return std::move(*reinterpret_cast<T *>(this));
#endif
  }

  template <typename T>
  T const &as() const {
    STATIC_ASSERT_RELATED(Base, T);

#ifdef DBG
    return *ASSERT_NOT_NULL(dynamic_cast<T const *>(base()));
#else
    return *reinterpret_cast<T const *>(this);
#endif
  }

 private:
  Base *base() { return static_cast<Base *>(this); }
  Base const *base() const { return static_cast<Base const *>(this); }
};

template <typename T>
std::unique_ptr<T> wrap_unique(T *ptr) {
  return std::unique_ptr<T>(ptr);
}

template <typename... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <typename... Ts>
overloaded(Ts...)->overloaded<Ts...>;
}  // namespace base
#undef STATIC_ASSERT_RELATED
#endif  // ICARUS_BASE_UTIL_H
