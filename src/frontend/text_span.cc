#include "text_span.h"
#include "../base/debug.h"

static void IncrementCursor(Source *source, Cursor *cursor) {
  if (cursor->offset != source->lines[cursor->line_num].size()) {
    ++cursor->offset;
  } else {
    ASSERT(source != nullptr);
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
