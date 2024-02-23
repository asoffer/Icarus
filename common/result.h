#ifndef ICARUS_COMMON_RESULT_H
#define ICARUS_COMMON_RESULT_H

#include <coroutine>

#include "nth/debug/debug.h"
#include "nth/base/configuration.h"
#include "nth/debug/source_location.h"
#include "nth/strings/interpolate.h"

namespace ic {
namespace internal_result {

template <nth::build>
struct DebugInfo {};

template <>
struct DebugInfo<nth::build::debug> {
  constexpr DebugInfo(nth::source_location loc) : location_(loc) {}

 protected:
  nth::source_location location_;
};
struct ResultPromiseReturnType;

}  // namespace internal_result

struct Result : internal_result::DebugInfo<nth::build_mode> {
  static constexpr Result success() { return Result(true); }

  template <typename = void>
  constexpr Result(bool value = false) requires(nth::build_mode !=
                                                nth::build::debug)
      : value_(value) {}

  template <typename = void>
  constexpr Result(
      bool value = false,
      nth::source_location loc =
          nth::source_location::current()) requires(nth::build_mode ==
                                                    nth::build::debug)
      : DebugInfo<nth::build_mode>(loc), value_(value) {}

  constexpr operator bool() const { return value_; }

  struct promise_type;

  bool await_ready() { return static_cast<bool>(*this); }
  void await_suspend(std::coroutine_handle<promise_type>);
  static constexpr void await_resume() {}

  friend void NthPrint(auto& p, auto& f, Result const& r) {
    if constexpr (nth::build_mode != nth::build::debug) {
      p.write(r.value_ ? "success" : "failure");
    } else {
      if (r.value_) {
        p.write("success");
      } else {
        nth::Interpolate<"[failure @ {}:{} ({})]">(
            p, f, r.location_.file_name(), r.location_.line(),
            r.location_.function_name());
      }
    }
  }


 private:
  friend internal_result::ResultPromiseReturnType;
  explicit Result(Result*& ptr_ref) : Result() { ptr_ref = this; }

  bool value_;
};

namespace internal_result {

struct ResultPromiseReturnType {
  ResultPromiseReturnType(Result::promise_type& promise);
  ResultPromiseReturnType(ResultPromiseReturnType&&)                 = delete;
  ResultPromiseReturnType(ResultPromiseReturnType const&)            = delete;
  ResultPromiseReturnType& operator=(ResultPromiseReturnType&&)      = delete;
  ResultPromiseReturnType& operator=(ResultPromiseReturnType const&) = delete;
  operator Result() {
    // TODO: This assumes eager conversion `get_return_object()` to the actual
    // result type.
    return Result(pointer_);
  }

 private:
  Result*& pointer_;
};

}  // namespace internal_result

struct Result::promise_type {
  internal_result::ResultPromiseReturnType get_return_object() { return *this; }
  static constexpr std::suspend_never initial_suspend() noexcept { return {}; }
  static constexpr std::suspend_never final_suspend() noexcept { return {}; }
  static constexpr void unhandled_exception() noexcept {}
  void return_value(Result value) { set(std::move(value)); }

  void set(Result result) { *value_ = std::move(result); }

 private:
  friend internal_result::ResultPromiseReturnType;
  Result* value_;
};

inline void Result::await_suspend(std::coroutine_handle<promise_type> h) {
  h.promise().set(std::move(*this));
  h.destroy();
}

namespace internal_result {

inline ResultPromiseReturnType::ResultPromiseReturnType(
    Result::promise_type& promise)
    : pointer_(promise.value_) {}

}  // namespace internal_result
}  // namespace ic

#endif  // ICARUS_COMMON_RESULT_H
