#ifndef ICARUS_BASE_GLOBAL_H
#define ICARUS_BASE_GLOBAL_H

#include "base/guarded.h"
#include "base/no_destructor.h"

namespace base {

// Global objects need to have all access guarded by some form of
// synchronization, and must never be destroyed. This class template ensures
// both of these criteria.
//
// TODO: initialization of globals is also complicated but not yet handled here.
template <typename T>
struct Global {
  explicit Global() {}
  constexpr Global(T&& val) : data_(std::forward<T>(val)) {}

  // Forwards `guarded`s lock handle.
  auto lock() { return data_->lock(); }

 private:
  NoDestructor<guarded<T>> data_;
};

// Constant globals should still not be destroyed but there's no need to lock
// them.
template <typename T>
struct Global<T const> {
  constexpr explicit Global() {}
  constexpr Global(T&& val) : data_(std::forward<T>(val)) {}

  T const* lock() { return &*data_; }

  T const* operator->() { return &*data_; }
  T const& operator*() { return *data_; }

 private:
  NoDestructor<T const> data_;
};

template <typename T>
Global(T)->Global<T const>;

}  // namespace base

#endif  // ICARUS_BASE_GLOBAL_H
