#ifndef ICARUS_COMMON_RESULT_H
#define ICARUS_COMMON_RESULT_H

#include <coroutine>

#include "nth/base/configuration.h"
#include "nth/debug/debug.h"
#include "nth/debug/source_location.h"
#include "nth/strings/interpolate.h"
#include "nth/utility/early_exit.h"

namespace ic {
namespace internal_result {

template <nth::build>
struct DebugInfo {};

template <>
struct DebugInfo<nth::build::debug> {
  constexpr DebugInfo(
      nth::source_location loc = nth::source_location::current())
      : location_(loc) {}

 protected:
  nth::source_location location_;
};
struct ResultPromiseReturnType;

}  // namespace internal_result

struct Result : internal_result::DebugInfo<nth::build_mode>,
                nth::early_exitable<Result> {
  using nth::early_exitable<Result>::early_exitable;

  static Result success() { return Result(true); }

  template <typename = void>
  Result(bool value = false) requires(nth::build_mode != nth::build::debug)
      : value_(value) {}

  template <typename = void>
  Result(bool value = false,
         nth::source_location loc =
             nth::source_location::current()) requires(nth::build_mode ==
                                                       nth::build::debug)
      : DebugInfo<nth::build_mode>(loc), value_(value) {}

  constexpr operator bool() const { return value_; }

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

  bool value_;
};

}  // namespace ic

#endif  // ICARUS_COMMON_RESULT_H
