#ifndef ICARUS_BASE_GUARDED_H
#define ICARUS_BASE_GUARDED_H

#include <memory>

#include "absl/synchronization/mutex.h"

namespace base {
template <typename T>
struct guarded {
  template <typename... Args>
  explicit guarded(Args&&... args) : val_(std::forward<Args>(args)...) {}

  auto lock() {
    mu_.Lock();
    auto deleter = [this](T*) { mu_.Unlock(); };
    return std::unique_ptr<T, decltype(deleter)>(&val_, deleter);
  }

 private:
  mutable absl::Mutex mu_;
  T val_;
};
}  // namespace base
#endif  // ICARUS_BASE_GUARDED_H
