#include "cursor.h"
#include "error_log.h"
#include "base/debug.h"

bool Cursor::MoveToNextLine() {
  ASSERT(source_file, "");
  auto next = source_file->NextLine();
  if (!next.first) {
    seen_eof_ = true;
    return false;
  }
  line = next.second;
  source_file->lines.push_back(line);
  offset = 0;
  ++line_num;
  return true;
}

Cursor Cursor::Behind(const Cursor &cursor, u64 dist) {
  ASSERT(cursor.offset >= dist, "");
  Cursor result = cursor;
  result.offset -= dist;
  return result;
}
