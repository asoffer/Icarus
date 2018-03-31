#ifndef ICARUS_BASE_GUARDED_H
#define ICARUS_BASE_GUARDED_H
#include <mutex>

namespace base {
template <typename T>
struct guarded {
  template <typename... Args>
  explicit guarded(Args&&...args) : val_(std::forward<Args>(args)...) {}

  auto lock() {
    mu_.lock();
    auto deleter = [this](T*) { mu_.unlock(); };
    return std::unique_ptr<T, decltype(deleter)>(&val_, deleter);
  }

 private:
  mutable std::mutex mu_;
  T val_;
};
}  // namespace base
#endif  // ICARUS_BASE_GUARDED_H
