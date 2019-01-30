#ifndef ICARUS_BASE_LAZY_H
#define ICARUS_BASE_LAZY_H

#include <mutex>

namespace base {

template <typename T>
struct lazy {
  lazy() : val_(nullptr){};

  T get() const { return val_; }

  template <typename Fn>
  void init(Fn&& f) const {
    std::call_once(flag_, [&]() { val_ = f(); });
  }

 private:
  mutable std::once_flag flag_;
  mutable T val_;
};

}  // namespace base

#endif  // ICARUS_BASE_LAZY_H
