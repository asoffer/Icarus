#include "cursor.h"
#include "error_log.h"
#include "base/debug.h"

void Cursor::Increment() {
  if (**this != '\0') {
    ++offset;
  } else {
    ASSERT(source_file, "");
    auto next = source_file->NextLine();
    if (next.eof) {
      seen_eof_ = true;
    } else {
      line = next.text;
      source_file->lines.push_back(line);
      offset = 0;
      ++line_num;
    }
  }
}

Cursor Cursor::Behind(const Cursor &cursor, u64 dist) {
  ASSERT_GE(cursor.offset, dist);
  Cursor result = cursor;
  result.offset -= dist;
  return result;
}
