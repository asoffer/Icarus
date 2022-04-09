#ifndef ICARUS_BASE_ITERATOR_H
#define ICARUS_BASE_ITERATOR_H

#include <utility>

namespace base {

template <typename B, typename E>
struct iterator_range {
  iterator_range(B b, E e) : begin_(std::move(b)), end_(std::move(e)) {}

  B begin() const { return begin_; }
  E end() const { return end_; }

  bool empty() const { return begin_ == end_; }

 private:
  [[no_unique_address]] B begin_;
  [[no_unique_address]] E end_;
};

template <typename B, typename E>
iterator_range(B, E) -> iterator_range<B, E>;

}  // namespace base

#endif  // ICARUS_BASE_ITERATOR_H
