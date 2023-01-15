#ifndef ICARUS_BASE_PTR_SPAN_H
#define ICARUS_BASE_PTR_SPAN_H

#include <concepts>
#include <memory>
#include <type_traits>

#include "base/meta.h"

namespace base {

// PtrSpan<T>:
// This is a utility that allows iterating over contiguous ranges of
// `std::unique_ptr<T>`s, giving access to the underlying `T` without needing
// the user to be concerned with the precise ownership model.  This template
// is much like std::span, but automatically calls `.get()` on each contained
// element when accessed.
//
// Example usage:
//  void FrobnicateAllThings(base::PtrSpan<Things const> things) {
//    for (Thing const *thing : things) { Frobnicate(thing); }
//  }
//
//  std::vector<std::unique_ptr<Things>> v = GetThings();
//  FrobnicateAllThings(v);
//
template <typename T>
struct PtrSpan {
 private:
  using pointer_type =
      std::conditional_t<std::is_const_v<T>,
                         std::unique_ptr<std::remove_const_t<T>> const,
                         std::unique_ptr<T>>;

 public:
  using value_type = T *;


  struct const_iterator {
    T *operator*() { return ptr_->get(); }
    T *operator->() { return ptr_->get(); }
    const_iterator operator++() { return const_iterator{++ptr_}; }
    const_iterator operator++(int) { return const_iterator{ptr_++}; }
    const_iterator operator--() { return const_iterator{--ptr_}; }
    const_iterator operator--(int) { return const_iterator{ptr_--}; }
    friend const_iterator operator+(const_iterator lhs, ptrdiff_t rhs) {
      return const_iterator{lhs.ptr_ + rhs};
    }
    friend const_iterator operator+(ptrdiff_t lhs, const_iterator rhs) {
      return rhs + lhs;
    }
    friend const_iterator operator-(const_iterator lhs, ptrdiff_t rhs) {
      return const_iterator{lhs.ptr_ - rhs};
    }
    friend const_iterator operator-(ptrdiff_t lhs, const_iterator rhs) {
      return rhs + lhs;
    }

    friend bool operator==(const_iterator lhs, const_iterator rhs) {
      return lhs.ptr_ == rhs.ptr_;
    }
    friend bool operator!=(const_iterator lhs, const_iterator rhs) {
      return not(lhs == rhs);
    }

   private:
    friend struct PtrSpan<T>;
    explicit const_iterator(pointer_type *ptr) : ptr_(ptr) {}
    pointer_type *ptr_ = nullptr;
  };

  struct const_reverse_iterator {
    T *operator*() { return ptr_->get(); }
    T *operator->() { return ptr_->get(); }
    const_reverse_iterator operator++() {
      return const_reverse_iterator{--ptr_};
    }
    const_reverse_iterator operator++(int) {
      return const_reverse_iterator{ptr_--};
    }
    const_reverse_iterator operator--() {
      return const_reverse_iterator{++ptr_};
    }
    const_reverse_iterator operator--(int) {
      return const_reverse_iterator{ptr_++};
    }
    friend const_reverse_iterator operator+(const_reverse_iterator lhs, ptrdiff_t rhs) {
      return const_reverse_iterator{lhs.ptr_ - rhs};
    }
    friend const_reverse_iterator operator+(ptrdiff_t lhs, const_reverse_iterator rhs) {
      return rhs + lhs;
    }
    friend const_reverse_iterator operator-(const_reverse_iterator lhs, ptrdiff_t rhs) {
      return const_reverse_iterator{lhs.ptr_ + rhs};
    }
    friend const_reverse_iterator operator-(ptrdiff_t lhs, const_reverse_iterator rhs) {
      return rhs - lhs;
    }

    friend bool operator==(const_reverse_iterator lhs,
                           const_reverse_iterator rhs) {
      return lhs.ptr_ == rhs.ptr_;
    }
    friend bool operator!=(const_reverse_iterator lhs,
                           const_reverse_iterator rhs) {
      return not(lhs == rhs);
    }

   private:
    friend struct PtrSpan<T>;
    explicit const_reverse_iterator(pointer_type *ptr) : ptr_(ptr) {}
    pointer_type *ptr_ = nullptr;
  };

  auto cbegin() const { return const_iterator{ptr_}; }
  auto cend() const { return const_iterator{ptr_ + size_}; }
  auto begin() const { return cbegin(); }
  auto end() const { return cend(); }
  auto rbegin() const { return crbegin(); }
  auto rend() const { return crend(); }
  auto crbegin() const { return const_reverse_iterator{ptr_ + (size_ - 1)}; }
  auto crend() const { return const_reverse_iterator{ptr_ - 1}; }

  size_t size() const { return size_; }
  bool empty() const { return size_ == 0; }

  value_type front() const { return ptr_->get(); }
  value_type back() const { return ptr_[size_ - 1].get(); }

  void remove_prefix(size_t num) {
    ptr_ += num;
    size_ -= num;
  }

  std::unique_ptr<T> &get(size_t n) { return ptr_[n]; }
  std::unique_ptr<T> &get_front() { return get(0); }
  std::unique_ptr<T> &get_back() { return get(this->size() - 1); }

  value_type operator[](size_t n) const { return ptr_[n].get(); }

  template <typename Container,
            std::enable_if_t<
                not nth::type<std::decay_t<Container>>.template is_a<PtrSpan>(),
                int> = 0>
  PtrSpan(Container &&c)
      : ptr_(c.empty() ? nullptr : std::addressof(c[0])), size_(c.size()) {}

  template <PtrConvertibleTo<T> U>
  PtrSpan(PtrSpan<U> span)
      : ptr_(std::addressof(span.get_front())), size_(span.size()) {}

  template <std::contiguous_iterator Iter>
  explicit PtrSpan(Iter b, Iter e)
      : ptr_(std::addressof(*b)),
        size_(std::addressof(*e) - std::addressof(*b)) {}

  explicit PtrSpan(pointer_type *p, size_t num) : ptr_(p), size_(num) {}

 private:
  pointer_type *ptr_ = nullptr;
  size_t size_       = 0;
};

}  // namespace base

#endif  // ICARUS_BASE_PTR_SPAN_H
