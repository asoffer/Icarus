#ifndef ICARUS_BASE_LAZY_H
#define ICARUS_BASE_LAZY_H

#include <atomic>
#include <mutex>
#include "base/debug.h"

namespace base {

template <typename T>
struct lazy {
  lazy() : val_(nullptr){};

  T get() const {
    auto val = val_.load(std::memory_order_relaxed);
    ASSERT(val != nullptr);
    return val;
  }

  template <typename Fn>
  void init(Fn&& f) const {
    auto tmp = val_.load(std::memory_order_relaxed);
    std::atomic_thread_fence(std::memory_order_acquire);
    if (tmp != nullptr) { return; }
    std::unique_lock l(mtx_);
    tmp = val_.load(std::memory_order_relaxed);
    if (tmp != nullptr) { return; }
    tmp = std::forward<Fn>(f)();
    std::atomic_thread_fence(std::memory_order_release);
    val_.store(tmp, std::memory_order_relaxed);
  }

 private:
  mutable std::mutex mtx_;
  mutable std::atomic<T> val_;
};

}  // namespace base

#endif  // ICARUS_BASE_LAZY_H
