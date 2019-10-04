#ifndef ICARUS_BASE_INTERVAL_H
#define ICARUS_BASE_INTERVAL_H

#include <algorithm>
#include <vector>

#include "base/debug.h"

namespace base {
template <typename T>
struct Interval {
  static_assert(
      std::is_same_v<bool, decltype(std::declval<T>() <= std::declval<T>())>);

  ICARUS_CONSTEXPR Interval(T b, T e)
      : begin_(std::move(b)), end_(std::move(e)) {
    ASSERT(b <= e);
  }

  // TODO make these conditionally constexpr
  constexpr T begin() const { return begin_; }
  constexpr T end() const { return end_; }

  // TODO I'm not sure these are good ideas.
  constexpr T &begin() { return begin_; }
  constexpr T &end() { return end_; }

  constexpr bool empty() const { return begin_ == end_; }

  template <typename U>
  Interval expanded(U &&val) {
    return Interval<T>(begin_ - std::forward<U>(val),
                       end_ + std::forward<U>(val));
  }

  Interval clamped_below(T &&val) {
    using std::max;
    return Interval<T>(max(std::forward<T>(val), begin_), end_);
  }

 private:
  T begin_;
  T end_;
};

template <typename T>
struct IntervalSet {
  IntervalSet() = default;
  IntervalSet(std::initializer_list<base::Interval<T>> intervals) {
    for (const auto &interval : intervals) { insert(interval); }
  }

 private:
  using container_type = std::vector<T>;

 public:
  struct const_iterator {
    const_iterator operator++() { return ++++iter_; }
    const_iterator operator++(int) { return iter_++ ++; }

    Interval<T> operator*() const { return Interval<T>(*iter_, *(iter_ + 1)); }

    friend bool operator==(const_iterator lhs, const_iterator rhs) {
      return lhs.iter_ == rhs.iter_;
    }
    friend bool operator!=(const_iterator lhs, const_iterator rhs) {
      return !(lhs == rhs);
    }

   private:
    friend struct IntervalSet;
    const_iterator(typename container_type::const_iterator i) : iter_(i) {}
    typename container_type::const_iterator iter_;
  };

  const_iterator begin() const { return const_iterator(endpoints_.begin()); }
  const_iterator end() const { return const_iterator(endpoints_.end()); }

  void insert(base::Interval<T> const &i) {
    auto lower =
        std::lower_bound(endpoints_.begin(), endpoints_.end(), i.begin());
    auto upper =
        std::upper_bound(endpoints_.begin(), endpoints_.end(), i.end());

    if (std::distance(lower, upper) == 0) {
      std::vector<T> entries;
      entries.push_back(i.begin());
      entries.push_back(i.end());
      endpoints_.insert(lower, entries.begin(), entries.end());
      return;
    }

    if (std::distance(endpoints_.begin(), lower) % 2 == 0) {
      *lower = i.begin();
      ++lower;
    }
    if (std::distance(endpoints_.begin(), upper) % 2 == 0) {
      --upper;
      *upper = i.end();
    }
    endpoints_.erase(lower, upper);
  }

  container_type endpoints_;
};
}  // namespace base

#endif  // ICARUS_BASE_INTERVAL_H
