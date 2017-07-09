#ifndef ICARUS_BASE_OWNED_PTR_H

#define ICARUS_BASE_OWNED_PTR_H

#include <type_traits>
#include <memory>

#include "debug.h"
#include "util.h"

namespace base {
template <typename T> struct owned_ptr {
public:
  owned_ptr(T* ptr = nullptr) : value_(ptr) {}

  template <typename D>
  owned_ptr(const owned_ptr<D> &ptr)
      : value_(ptr ? nullptr : new T(*ptr.get())) {}
  template <typename D> owned_ptr(owned_ptr<D> &&ptr) : value_(ptr.release()) {}

  ~owned_ptr() { delete value_; }

  operator bool() const { return value_ != nullptr; }

  T *operator->() const { return value_; }
  T &operator*() { return *value_; }
  const T &operator*() const { return *value_; }

  void operator=(const owned_ptr &ptr) {
    if (value_) {
      if (ptr) {
        *value_ = *ptr;
      } else {
        delete value_;
        value_ = nullptr;
      }
    } else if (ptr) {
      value_ = new T(*ptr);
    }
  }

  template<typename D>
  void operator=(const owned_ptr<D> &ptr) {
    if (value_) {
      if (ptr) {
        *value_ = *ptr;
      } else {
        delete value_;
        value_ = nullptr;
      }
    } else if (ptr) {
      value_ = new T(*ptr);
    }
  }

  void operator=(owned_ptr&& ptr) {
    if (value_) { delete value_; }
    value_ = ptr.release();
  }

  template <typename D>
  void operator=(owned_ptr<D>&& ptr) {
    if (value_) { delete value_; }
    value_ = ptr.release();
  }


  T* get() const { return value_; }

  T* release() { 
    auto val = value_;
    value_ = nullptr;
    return val;
  }

private:
  T *value_ = nullptr;
};

template <typename T, typename... Args> owned_ptr<T> make_owned(Args... args) {
  return owned_ptr<T>(new T(std::forward<Args>(args)...));
}
template <typename T> owned_ptr<T> own(T *ptr) { return owned_ptr<T>(ptr); }

template <typename To, typename From>
base::owned_ptr<To> move(base::owned_ptr<From> &&ptr) {
  return own(ptr_cast<To>(ptr.release()));
}

template <typename To, typename From>
base::owned_ptr<To> move(base::owned_ptr<From> &ptr) {
  return own(ptr_cast<To>(ptr.release()));
}
} // namespace base

#endif // ICARUS_BASE_OWNED_PTR_H
