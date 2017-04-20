#ifndef ICARUS_CURSOR_H
#define ICARUS_CURSOR_H

#include "base/types.h"
#include "base/debug.h"
#include "base/file.h"
#include "util/pstr.h"

#include <vector>
#include <string>

struct Cursor {
  Cursor() {}

  pstr line;
  size_t offset = 0;
  size_t line_num = 0;
  File *source_file = nullptr;

  std::string file_name() const { return source_file->name; }

  // Get the character that the cursor is currently pointing to
  char &operator*(void)const { return *(line.ptr + offset); }

  void MoveToNextLine();

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

  void Increment() {
    if (**this != '\0') {
      ++offset;
    } else {
      MoveToNextLine();
    }
  }
};

#endif // ICARUS_CURSOR_H
