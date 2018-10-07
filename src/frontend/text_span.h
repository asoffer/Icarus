#ifndef ICARUS_FRONTEND_TEXT_SPAN_H
#define ICARUS_FRONTEND_TEXT_SPAN_H

#include "base/debug.h"
#include "base/types.h"
#include "frontend/source.h"

struct Cursor {
  u32 offset   = 0;
  u32 line_num = 0;
};

struct TextSpan {
  TextSpan() {}
  TextSpan(const Cursor &s, const Cursor &f) : start(s), finish(f) {}
  TextSpan(const TextSpan &s, const TextSpan &f);

  char last_char() const {
    return source->lines[finish.line_num][finish.offset];
  }
  void Increment();

  Cursor start;
  Cursor finish;
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

  void SkipToEndOfLine() { cursor.offset = static_cast<u32>(line().size()); }

  void BackUp() {
    // You can't back up to a previous line.
    ASSERT(cursor.offset > 0u);
    --cursor.offset;
  }

  void Increment();

  Cursor cursor;
  frontend::Source *source = nullptr;
};

#endif  // ICARUS_FRONTEND_TEXT_SPAN_H
