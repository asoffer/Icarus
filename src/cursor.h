#ifndef ICARUS_CURSOR_H
#define ICARUS_CURSOR_H

#include "base/types.h"
#include "base/debug.h"
#include "base/source.h"

#include <vector>
#include <string>

struct Cursor {
  Cursor() {}

  std::string line;
  size_t offset = 0;
  size_t line_num = 0;
  Source *source_file = nullptr;

  std::string file_name() const { return source_file->name; }

  // Get the character that the cursor is currently pointing to
  char &operator*(void) { return line[offset]; }

  static Cursor Behind(const Cursor &cursor, u64 dist);

  void SkipToEndOfLine() {
    while (**this != '\0') {
      ++offset;
    }
  }

  void BackUp() {
    // You can't back up to a previous line.
    ASSERT(offset > 0, "offset is negative.");
    --offset;
  }

  void Increment();

  bool seen_eof_ = false;
};

#endif // ICARUS_CURSOR_H
