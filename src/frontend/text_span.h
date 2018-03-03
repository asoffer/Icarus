#ifndef ICARUS_FRONTEND_TEXT_SPAN_H
#define ICARUS_FRONTEND_TEXT_SPAN_H

#include "../base/debug.h"
#include "../base/source.h"

struct Cursor {
  u32 offset   = 0;
  u32 line_num = 0;
};

struct TextSpan {
  TextSpan() {}
  TextSpan(const Cursor &s, const Cursor &f) : start(s), finish(f) {}
  TextSpan(const TextSpan &s, const TextSpan &f)
      : start(s.start), finish(f.finish), source(s.source) {
    ASSERT(s.source == f.source, "");
  }

  char last_char() const {
    return source->lines[finish.line_num][finish.offset];
  }
  void Increment();

  Cursor start;
  Cursor finish;
  Source *source = nullptr;
};

struct SourceLocation {
  // Get the character that the cursor is currently pointing to
  const char &operator*() const {
    return source->lines[cursor.line_num][cursor.offset];
  }
  const Source::Line &line() const { return source->lines[cursor.line_num]; }
  TextSpan ToSpan() const {
    TextSpan span(cursor, cursor);
    span.source = source;
    return span;
  }

  static SourceLocation Behind(const SourceLocation &cursor, u32 dist);

  void SkipToEndOfLine() { cursor.offset = static_cast<u32>(line().size()); }

  void BackUp() {
    // You can't back up to a previous line.
    ASSERT_GT(cursor.offset, 0u);
    --cursor.offset;
  }

  void Increment();

  Cursor cursor;
  Source *source = nullptr;
};

#endif // ICARUS_FRONTEND_TEXT_SPAN_H
