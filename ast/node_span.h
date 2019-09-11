#ifndef ICARUS_AST_NODE_SPAN_H
#define ICARUS_AST_NODE_SPAN_H

#include <memory>

namespace ast {
// Many AST nodes hold vectors of unique_ptrs to other nodes. We want to provide
// access to the raw pointers without needing the user to be concerned with the
// precise ownership model. To do this, we provide the following `NodeSpan`
// template which is much like absl::Span, but automatically calls `.get()` on
// each contained element when accessed.
//
// Example usage:
//  void FrobnicateAllNodes(NodeSpan<Expression const> nodes) {
//    for (Expression const *node : nodes) { Frobnicate(node); }
//  }
template <typename T>
struct NodeSpan {
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
    friend bool operator!=(iterator lhs, iterator rhs) { return !(lhs == rhs); }

   private:
    friend struct NodeSpan<T>;
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

  template <typename Container>
  NodeSpan(Container &&c)
      : ptr_(c.empty() ? nullptr : &c[0]), size_(c.size()) {}
  template <typename Iter>
  explicit NodeSpan(Iter b, Iter e)
      : ptr_(std::addressof(*b)),
        size_(std::addressof(*e) - std::addressof(*b)) {}

 private:
  pointer_type *ptr_ = nullptr;
  size_t size_       = 0;
};

}  // namespace ast

#endif  // ICARUS_AST_NODE_SPAN_H
