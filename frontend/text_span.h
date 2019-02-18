#ifndef ICARUS_FRONTEND_TEXT_SPAN_H
#define ICARUS_FRONTEND_TEXT_SPAN_H

#include "base/debug.h"
#include "base/interval.h"
#include "frontend/deprecated_source.h"

struct SourceCursor {
  uint32_t offset   = 0;
  uint32_t line_num = 0;
};

struct TextSpan {
  TextSpan() {}
  TextSpan(const SourceCursor &s, const SourceCursor &f) : start(s), finish(f) {}
  TextSpan(const TextSpan &s, const TextSpan &f);

  char last_char() const {
    return source->lines[finish.line_num][finish.offset];
  }
  void Increment();

  base::Interval<size_t> lines() const {
    return base::Interval<size_t>{start.line_num, finish.line_num + 1};
  }

  SourceCursor start;
  SourceCursor finish;
  frontend::Source *source = nullptr;
};

struct SourceLocation {
  // Get the character that the cursor is currently pointing to
  const char &operator*() const {
    return source->lines[cursor.line_num][cursor.offset];
  }
  const frontend::Source::Line &line() const {
    return source->lines[cursor.line_num];
  }
  TextSpan ToSpan() const {
    TextSpan span(cursor, cursor);
    span.source = source;
    return span;
  }

  void SkipToEndOfLine() { cursor.offset = static_cast<uint32_t>(line().size()); }

  void BackUp() {
    // You can't back up to a previous line.
    ASSERT(cursor.offset > 0u);
    --cursor.offset;
  }

  void Increment();

  SourceCursor cursor;
  frontend::Source *source = nullptr;
};

#endif  // ICARUS_FRONTEND_TEXT_SPAN_H
