#ifndef ICARUS_BASE_NO_DESTRUCTOR_H
#define ICARUS_BASE_NO_DESTRUCTOR_H

#include <utility>

namespace base {
template <typename T>
struct alignas(T) NoDestructor {
 public:
  template <typename... Args>
  NoDestructor(Args &&... args) {
    new (buf_) T(std::forward<Args>(args)...);
  }

  T const &operator*() const & { return *reinterpret_cast<T const *>(buf_); }
  T &operator*() & { return *reinterpret_cast<T *>(buf_); }
  T &&operator*() && { return static_cast<T &&>(*reinterpret_cast<T *>(buf_)); }
  T const &&operator*() const && {
    return static_cast<T const &&>(*reinterpret_cast<T const *>(buf_));
  }

  T const *operator->() const { return reinterpret_cast<T const *>(buf_); }
  T *operator->() { return reinterpret_cast<T *>(buf_); }

 private:
  char buf_[sizeof(T)];
};

template <typename T>
NoDestructor(T)->NoDestructor<T>;

}  // namespace base

#endif  // ICARUS_BASE_NO_DESTRUCTOR_H
