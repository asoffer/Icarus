#include "cursor.h"
#include "base/debug.h"

void Cursor::Increment() {
  if (offset != line().size()) {
    ++offset;
  } else {
    ASSERT(source != nullptr, "");
    auto next = source->NextLine();
    if (!next) {
      seen_eof_ = true;
    } else {
      source->lines.push_back(std::move(*next));
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
