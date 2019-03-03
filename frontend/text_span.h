#ifndef ICARUS_FRONTEND_TEXT_SPAN_H
#define ICARUS_FRONTEND_TEXT_SPAN_H

#include "base/debug.h"
#include "base/interval.h"

namespace frontend {
struct Src;
}

struct SourceCursor {
  uint32_t offset   = 0;
  uint32_t line_num = 0;
};

struct TextSpan {
  TextSpan() {}
  TextSpan(SourceCursor const &s, SourceCursor const &f)
      : start(s), finish(f) {}
  TextSpan(TextSpan const &s, TextSpan const &f)
      : start(s.start), finish(f.finish), source(s.source) {}

  base::Interval<size_t> lines() const {
    return base::Interval<size_t>{start.line_num, finish.line_num + 1};
  }

  SourceCursor start;
  SourceCursor finish;
  frontend::Src *source = nullptr;
};

#endif  // ICARUS_FRONTEND_TEXT_SPAN_H
