#ifndef ICARUS_BASE_DEBUG_H
#define ICARUS_BASE_DEBUG_H

#include <sstream>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_format.h"
#include "base/log.h"
#include "nth/io/string_printer.h"
#include "nth/strings/format/universal.h"
#include "nth/meta/type.h"

#if defined(ICARUS_DEBUG)

#define ASSERT(...)                                                            \
  ([](auto const& value) {                                                     \
    if (not value.result) {                                                    \
      ::debug::internal::LogAndAbort(value, #__VA_ARGS__);                     \
    }                                                                          \
  }(::debug::internal::Stealer{} << __VA_ARGS__                                \
                                 << ::debug::internal::Stealer{}));

namespace debug::internal {

template <typename L, typename R>
struct AssertionResult {
  bool result;
  L const& l;
  R const& r;
};

template <typename T>
struct AssertionResult<T, void> {
  bool result;
  T const& t;
};

struct StolenBase {};
template <typename T>
struct Stolen : StolenBase {
  using value_type = T;
  explicit Stolen(T const& value) : value_(value) {}

  T const& value() const { return value_; }

 private:
  T const& value_;
};

struct Stealer {
  template <typename T>
  friend Stolen<T> operator<<(Stealer, T const& value) {
    return Stolen<T>(value);
  }

  template <typename T>
  friend auto operator<<(T const& value, Stealer) {
    if constexpr (std::is_base_of_v<StolenBase, T>) {
      return AssertionResult<typename T::value_type, void>{
          .result = static_cast<bool>(value.value()),
          .t      = value.value(),
      };
    } else {
      return Stolen<T>(value);
    }
  }
};

template <typename L, typename R>
AssertionResult<L, R> operator==(Stolen<L> l, Stolen<R> r) {
  return {.result = l.value() == r.value(), .l = l.value(), .r = r.value()};
}

template <typename L, typename R>
AssertionResult<L, R> operator!=(Stolen<L> l, Stolen<R> r) {
  return {.result = l.value() != r.value(), .l = l.value(), .r = r.value()};
}

template <typename L, typename R>
AssertionResult<L, R> operator>(Stolen<L> l, Stolen<R> r) {
  return {.result = l.value() > r.value(), .l = l.value(), .r = r.value()};
}

template <typename L, typename R>
AssertionResult<L, R> operator>=(Stolen<L> l, Stolen<R> r) {
  return {.result = l.value() >= r.value(), .l = l.value(), .r = r.value()};
}

template <typename L, typename R>
AssertionResult<L, R> operator<(Stolen<L> l, Stolen<R> r) {
  return {.result = l.value() < r.value(), .l = l.value(), .r = r.value()};
}

template <typename L, typename R>
AssertionResult<L, R> operator<=(Stolen<L> l, Stolen<R> r) {
  return {.result = l.value() <= r.value(), .l = l.value(), .r = r.value()};
}

auto wrap(auto const & arg) -> decltype(auto) {
  static constexpr auto type = nth::type<decltype(arg)>.decayed();
  if constexpr (type == nth::type<char const*>) {
    return std::string_view(arg);
  } else if constexpr (type == nth::type<std::nullptr_t>) {
    return std::string_view("null");
  } else if constexpr (std::is_pointer_v<nth::type_t<type>>) {
    return absl::StrFormat("%p", arg);
  } else if constexpr (std::is_enum_v<nth::type_t<type>>) {
    return absl::StrFormat("(%s)%d", typeid(type).name(),
                           static_cast<int>(arg));
  } else if constexpr (requires { absl::StrCat(arg); }) {
    return arg;
  } else {
    return absl::StrCat("unknown value of type ", typeid(decltype(arg)).name());
  }
}

template <typename L, typename R>
[[noreturn]] void LogAndAbort(
    AssertionResult<L, R> assertion_result, std::string_view expression,
    std::experimental::source_location src_loc =
        std::experimental::source_location::current()) {
  int n;
  if constexpr (std::is_void_v<R>) {
    base::internal_logging::Log(
        ::base::internal_logging::kLogWithoutFunctionNameFormat, src_loc, "",
        "\e[0;1;31mAssertion failed\n"
        "    \e[0;1;37mExpected:\e[0m %s\n",
        expression);
  } else {
    base::internal_logging::Log(
        ::base::internal_logging::kLogWithoutFunctionNameFormat, src_loc, "",
        "\e[0;1;31mAssertion failed\n"
        "    \e[0;1;37mExpected:\e[0m %s\n"
        "    \e[0;1;37m     LHS:\e[0m %v\n"
        "    \e[0;1;37m     RHS:\e[0m %v\n",
        expression, ::debug::internal::wrap(assertion_result.l),
        ::debug::internal::wrap(assertion_result.r));
  }
  std::abort();
}

}  // namespace debug::internal

#define ASSERT_NOT_NULL(expr)                                                  \
  ([](auto&& ptr,                                                              \
      std::experimental::source_location src_loc) -> decltype(auto) {          \
    if (ptr == nullptr) {                                                      \
      LOG("", "%s is unexpectedly null.", #expr);                              \
      ::std::abort();                                                          \
    }                                                                          \
    return static_cast<std::remove_reference_t<decltype(ptr)>&&>(ptr);         \
  })(expr, std::experimental::source_location::current())

#define UNREACHABLE(...)                                                       \
  do {                                                                         \
    LOG("", "Unreachable code-path.\n%s", [&](auto&&... args) {                \
      std::string s;                                                           \
      absl::StrAppend(&s, args...);                                            \
      return s;                                                                \
    }(__VA_ARGS__));                                                           \
    std::abort();                                                              \
  } while (false)

#else  // defined(ICARUS_DEBUG)

#define ASSERT(...) static_assert(true)
#define ASSERT_NOT_NULL(...) (__VA_ARGS__)
#define UNREACHABLE(...) __builtin_unreachable();

#endif  // defined(ICARUS_DEBUG)

#define NOT_YET(...)                                                           \
  do {                                                                         \
    LOG("", "Not yet implemented.\n%s", [&](auto&&... args) {                  \
      std::string s;                                                           \
      absl::StrAppend(&s, args...);                                            \
      return s;                                                                \
    }(__VA_ARGS__));                                                           \
    std::abort();                                                              \
  } while (false)

#endif  // ICARUS_BASE_DEBUG_H
