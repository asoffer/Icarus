#ifndef ICARUS_BASE_ITERATOR_H
#define ICARUS_BASE_ITERATOR_H

namespace base {

template <typename I>
struct iterator_range {
  iterator_range(I b, I e) : begin_(b), end_(e) {}

  I begin() const { return begin_; }
  I end() const { return end_; }

 private:
  I begin_, end_;
};

}  // namespace base

#endif  // ICARUS_BASE_ITERATOR_H
