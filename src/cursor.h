#ifndef ICARUS_CURSOR_H
#define ICARUS_CURSOR_H

#include "base/debug.h"
#include "base/source.h"

#include <string>

struct Cursor {
  Cursor() {}

  size_t offset   = 0;
  size_t line_num = 0;
  Source *source  = nullptr;

  // Get the character that the cursor is currently pointing to
  char operator*() { return source->lines[line_num][offset]; }
  const Source::Line &line() const { return source->lines[line_num]; }

  static Cursor Behind(const Cursor &cursor, u64 dist);

  void SkipToEndOfLine() {
    while (**this != '\0') {
      ++offset;
    }
  }

  void BackUp() {
    // You can't back up to a previous line.
    ASSERT_GT(offset, 0);
    --offset;
  }

  void Increment();

  bool seen_eof_ = false;
};

#endif // ICARUS_CURSOR_H
