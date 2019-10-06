#ifndef ICARUS_FRONTEND_SOURCE_RANGE_H
#define ICARUS_FRONTEND_SOURCE_RANGE_H

#include <cstdint>

#include "base/strong_types.h"
#include "base/debug.h"
#include "base/interval.h"

namespace frontend {

ICARUS_BASE_DEFINE_STRONG_TYPE(LineNum, uint32_t{0},  //
                               base::EnableRawArithmetic,
                               base::EnableComparisons);
ICARUS_BASE_DEFINE_STRONG_TYPE(Offset, uint32_t{0},  //
                               base::EnableRawArithmetic,
                               base::EnableComparisons);

struct SourceLoc {
  constexpr SourceLoc() = default;
  explicit constexpr SourceLoc(LineNum l, Offset o) : line_num(l), offset(o) {}

  LineNum line_num = LineNum(0);
  Offset offset    = Offset(0);

  constexpr SourceLoc next_line() const { return SourceLoc(line_num + 1, Offset(0)); }

  constexpr SourceLoc &operator+=(Offset o) {
    offset += o;
    return *this;
  }

  constexpr SourceLoc &operator-=(Offset o) {
    offset -= o;
    return *this;
  }

 private:
  constexpr uint64_t value() const {
    return (uint64_t{line_num.value} << 32) | offset.value;
  }

  friend constexpr bool operator<(SourceLoc const &lhs, SourceLoc const &rhs) {
    return lhs.value() < rhs.value();
  }
  friend constexpr bool operator==(SourceLoc const &lhs, SourceLoc const &rhs) {
    return lhs.value() == rhs.value();
  }
};

constexpr SourceLoc operator+(SourceLoc loc, Offset offset) {
  loc += offset;
  return loc;
}

constexpr SourceLoc operator+(Offset offset, SourceLoc loc) { return loc + offset; }

constexpr SourceLoc operator-(SourceLoc loc, Offset offset) {
  loc -= offset;
  return loc;
}

constexpr SourceLoc operator-(Offset offset, SourceLoc loc) { return loc - offset; }

constexpr bool operator>(SourceLoc const &lhs, SourceLoc const &rhs) {
  return rhs < lhs;
}

constexpr bool operator<=(SourceLoc const &lhs, SourceLoc const &rhs) {
  return !(lhs > rhs);
}

constexpr bool operator>=(SourceLoc const &lhs, SourceLoc const &rhs) {
  return !(lhs < rhs);
}

constexpr bool operator!=(SourceLoc const &lhs, SourceLoc const &rhs) {
  return !(lhs == rhs);
}

struct SourceRange {
  constexpr SourceRange() : range_(SourceLoc(), SourceLoc()) {}
  explicit constexpr SourceRange(SourceLoc const &b, SourceLoc const &e) : range_(b, e) {}

  // TODO remove this constructor.
  explicit constexpr SourceRange(SourceRange const &b, SourceRange const &e)
      : range_(b.begin(), e.end()) {}

  base::Interval<LineNum> lines() const {
    return base::Interval<LineNum>(begin().line_num, end().line_num + 1);
  }

  constexpr SourceRange expanded(Offset o) {
    return SourceRange(begin() - o, end() + o);
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
