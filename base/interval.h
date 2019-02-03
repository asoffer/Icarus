#ifndef ICARUS_BASE_INTERVAL_H
#define ICARUS_BASE_INTERVAL_H

#include "base/container/vector.h"
#include <algorithm>

namespace base {
template <typename T>
struct Interval {
  T start_;
  T end_;

  template <typename U>
  Interval expanded(U &&val) {
    return Interval{start_ - std::forward<U>(val), end_ + std::forward<U>(val)};
  }

  Interval clamped_below(T &&val) {
    return Interval{std::max<T>(std::forward<T>(val), start_), end_};
  }
};

template <typename T>
struct IntervalSet {
  IntervalSet() = default;
  IntervalSet(std::initializer_list<base::Interval<T>> intervals) {
    for (const auto &interval : intervals) { insert(interval); }
  }

  void insert(base::Interval<T> const &i) {
    auto lower =
        std::lower_bound(endpoints_.begin(), endpoints_.end(), i.start_);
    auto upper = std::upper_bound(endpoints_.begin(), endpoints_.end(), i.end_);

    if (std::distance(lower, upper) == 0) {
      auto entries = base::vector<T>{i.start_, i.end_};
      endpoints_.insert(lower, entries.begin(), entries.end());
      return;
    }

    if (std::distance(endpoints_.begin(), lower) % 2 == 0) {
      *lower = i.start_;
      ++lower;
    }
    if (std::distance(endpoints_.begin(), upper) % 2 == 0) {
      --upper;
      *upper = i.end_;
    }
    endpoints_.erase(lower, upper);
  }

  base::vector<T> endpoints_;
};
}  // namespace base

#endif  // ICARUS_BASE_INTERVAL_H
