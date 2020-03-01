#ifndef ICARUS_BASE_LAZY_H
#define ICARUS_BASE_LAZY_H

#include <mutex>

#include "base/debug.h"

namespace base {

template <typename T>
struct  alignas(T) lazy {
  T get() const { return *reinterpret_cast<T*>(val_); }
  ~lazy() {
    bool was_called = true;
    std::call_once(flag_, [&] { was_called = false; });
    if (was_called) { reinterpret_cast<T*>(val_)->~T(); }
  }

  template <typename Fn>
  void init(Fn&& f) const {
    std::call_once(flag_, [&]() { new (val_) T(std::forward<Fn>(f)()); });
  }

 private:
  mutable char val_[sizeof(T)] = {};
  mutable std::once_flag flag_;
};

}  // namespace base

#endif  // ICARUS_BASE_LAZY_H
