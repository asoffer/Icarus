#ifndef ICARUS_BASE_PTR_SPAN_H
#define ICARUS_BASE_PTR_SPAN_H

#include <memory>

namespace base {

// PtrSpan<T>:
// This is a utility that allows iterating over contiguous ranges of
// `std::unique_ptr<T>`s, giving access to the underlying `T` without needing
// the user to be concerned with the precise ownership model.  This template
// is much like absl::Span, but automatically calls `.get()` on each contained
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
  struct iterator {
    T *operator*() { return ptr_->get(); }
    T *operator->() { return ptr_->get(); }
    iterator operator++() { return iterator{++ptr_}; }
    iterator operator++(int) { return iterator{ptr_++}; }

    friend bool operator==(iterator lhs, iterator rhs) {
      return lhs.ptr_ == rhs.ptr_;
    }
    friend bool operator!=(iterator lhs, iterator rhs) {
      return not(lhs == rhs);
    }

   private:
    friend struct PtrSpan<T>;
    explicit iterator(pointer_type *ptr) : ptr_(ptr) {}
    pointer_type *ptr_ = nullptr;
  };

  iterator begin() const { return iterator{ptr_}; }
  iterator end() const { return iterator{ptr_ + size_}; }
  size_t size() const { return size_; }
  bool empty() const { return size_ == 0; }

  T *front() const { return ptr_->get(); }
  T *back() const { return ptr_[size_ - 1].get(); }

  std::unique_ptr<T> &get(size_t n) { return ptr_[n]; }
  std::unique_ptr<T> &get_front() { return get(0); }
  std::unique_ptr<T> &get_back() { return get(this->size() - 1); }

  T *operator[](size_t n) const { return ptr_[n].get(); }

  template <
      typename Container,
      std::enable_if_t<not std::is_same_v<PtrSpan<T>, std::decay_t<Container>>>
          * = nullptr>
  PtrSpan(Container &&c)
      : ptr_(c.empty() ? nullptr : std::addressof(c[0])), size_(c.size()) {}
  template <typename Iter>
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
