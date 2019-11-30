#ifndef ICARUS_BASE_MOVE_FUNC_H
#define ICARUS_BASE_MOVE_FUNC_H

#include <functional>

namespace base {

template <typename Fn>
struct move_func;

template <typename R, typename... Args>

struct move_func<R(Args...)> {
  move_func() noexcept : fn_(nullptr) {}
  move_func(std::nullptr_t) noexcept : fn_(nullptr) {}

  template <typename F>
  move_func(F&& f) : fn_(std::forward<F>(f)) {}

  move_func(move_func const&) = delete;
  move_func(move_func&&)      = default;

  move_func& operator=(move_func const&) = delete;
  move_func& operator=(move_func&&) = default;

  R operator()(Args... args) && {
    return std::exchange(fn_, nullptr)(std::forward<Args>(args)...);
  }

  explicit operator bool() const noexcept { return fn_ == nullptr; }

  friend bool operator==(move_func const& lhs, move_func const& rhs) {
    return lhs.fn_ == rhs.fn_;
  }
  friend bool operator==(move_func const& f, std::nullptr_t) {
    return not static_cast<bool>(f.fn_);
  }
  friend bool operator==(std::nullptr_t, move_func const& f) {
    return not static_cast<bool>(f.fn_);
  }
  friend bool operator!=(move_func const& lhs, move_func const& rhs) {
    return not(lhs == rhs);
  }
  friend bool operator!=(move_func const& f, std::nullptr_t) {
    return not(f == nullptr);
  }
  friend bool operator!=(std::nullptr_t, move_func const& f) {
    return not(nullptr == f);
  }

 private:
  std::function<R(Args...)> fn_;
};

}  // namespace base

#endif  // ICARUS_BASE_MOVE_FUNC_H
