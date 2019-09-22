#ifndef ICARUS_FRONTEND_SOURCE_RANGE_H
#define ICARUS_FRONTEND_SOURCE_RANGE_H

#include <cstdint>

#include "base/debug.h"
#include "base/interval.h"

namespace frontend {

struct SourceLoc {
  constexpr SourceLoc() = default;
  constexpr SourceLoc(uint32_t l, uint32_t o) : line_num(l), offset(o) {}

  uint32_t line_num = 0;
  uint32_t offset   = 0;
};

constexpr bool operator<(SourceLoc const &lhs, SourceLoc const &rhs) {
  return ((uint64_t{lhs.line_num} << 32) | lhs.offset) <
         ((uint64_t{rhs.line_num} << 32) | rhs.offset);
}

constexpr bool operator>(SourceLoc const &lhs, SourceLoc const &rhs) {
  return rhs < lhs;
}

constexpr bool operator<=(SourceLoc const &lhs, SourceLoc const &rhs) {
  return !(lhs > rhs);
}

constexpr bool operator>=(SourceLoc const &lhs, SourceLoc const &rhs) {
  return !(lhs < rhs);
}

constexpr bool operator==(SourceLoc const &lhs, SourceLoc const &rhs) {
  return ((uint64_t{lhs.line_num} << 32) | lhs.offset) ==
         ((uint64_t{rhs.line_num} << 32) | rhs.offset);
}

constexpr bool operator!=(SourceLoc const &lhs, SourceLoc const &rhs) {
  return !(lhs == rhs);
}

struct SourceRange {
  SourceRange() : range_(SourceLoc(), SourceLoc()) {}
  SourceRange(SourceLoc const &b, SourceLoc const &e) : range_(b, e) {}

  // TODO remove this constructor.
  SourceRange(SourceRange const &b, SourceRange const &e)
      : range_(b.begin(), e.end()) {}

  base::Interval<size_t> lines() const {
    return base::Interval<size_t>{begin().line_num, end().line_num + 1};
  }

  constexpr SourceLoc begin() const { return range_.begin(); }
  constexpr SourceLoc end() const { return range_.end(); }

  constexpr SourceLoc &begin() { return range_.begin(); }
  constexpr SourceLoc &end() { return range_.end(); }

 private:
  base::Interval<SourceLoc> range_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_RANGE_H
