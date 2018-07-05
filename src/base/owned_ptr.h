#ifndef ICARUS_BASE_OWNED_PTR_H
#define ICARUS_BASE_OWNED_PTR_H

#include <memory>

namespace base {
template <typename T>
struct owned_ptr {
  owned_ptr() noexcept = default;
  owned_ptr(owned_ptr const& ptr) noexcept
      : ptr_(ptr == nullptr ? nullptr : ptr->Clone()) {}
  owned_ptr(owned_ptr&& ptr) noexcept : ptr_(std::move(ptr).ptr_) {}

  T* operator->() { return ptr_.get(); }
  T const* operator->() const { return ptr_.get(); }
  T& operator*() { return *ptr_; }
  T const& operator*() const { return *ptr_; }

  owned_ptr& operator=(const owned_ptr& ptr) noexcept {
    ptr_.reset(ptr.get() == nullptr ? nullptr : ptr->Clone());
    return *this;
  }
  owned_ptr& operator=(owned_ptr&& ptr) noexcept {
    ptr_ = std::move(ptr).ptr_;
    return *this;
  }
  owned_ptr& operator=(std::unique_ptr<T> ptr) noexcept {
    ptr_ = std::move(ptr);
    return *this;
  }

  template <typename D>
  owned_ptr& operator=(owned_ptr<D>&& ptr) noexcept {
    ptr_ = std::move(ptr).ptr_;
    return *this;
  }
  template <typename D>
  owned_ptr& operator=(std::unique_ptr<D>&& ptr) noexcept {
    ptr_ = std::move(ptr);
    return *this;
  }

  T const* get() const { return ptr_.get(); }
  T* get() { return ptr_.get(); }

  std::unique_ptr<T> ptr_;
};

template <typename T, typename... Args>
owned_ptr<T> make_owned(Args&&... args) {
  owned_ptr<T> ptr;
  ptr.ptr_ = std::make_unique<T>(std::forward<Args>(args)...);
  return ptr;
}

template <typename T>
bool operator==(const owned_ptr<T>& ptr, std::nullptr_t) {
  return ptr.get() == nullptr;
}

template <typename T>
bool operator==(std::nullptr_t, const owned_ptr<T>& ptr) {
  return ptr.get() == nullptr;
}
}  // namespace base
#endif  // ICARUS_BASE_OWNED_PTR_H
