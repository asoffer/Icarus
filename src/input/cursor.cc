#include "cursor.h"
#include "../base/debug.h"

static void IncrementCursor(Source *source, Cursor *cursor) {
  if (cursor->offset != source->lines[cursor->line_num].size()) {
    ++cursor->offset;
  } else {
    ASSERT(source != nullptr, "");
    auto next = source->NextLine();
    if (!next) {
      source->seen_eof = true;
    } else {
      source->lines.push_back(std::move(*next));
      cursor->offset = 0;
      ++cursor->line_num;
    }
  }
}

void TextSpan::Increment() { IncrementCursor(source, &finish); }
void SourceLocation::Increment() { IncrementCursor(source, &cursor); }

SourceLocation SourceLocation::Behind(const SourceLocation &loc, u32 dist) {
  ASSERT_GE(loc.cursor.offset, dist);
  SourceLocation result = loc;
  result.cursor.offset -= dist;
  return result;
}
