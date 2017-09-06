#include "cursor.h"
#include "../base/debug.h"

void SourceLocation::Increment() {
  if (**this != '\0') {
    ++cursor.offset;
  } else {
    ASSERT(source != nullptr, "");
    auto next = source->NextLine();
    if (!next) {
      seen_eof_ = true;
    } else {
      source->lines.push_back(std::move(*next));
      cursor.offset = 0;
      ++cursor.line_num;
    }
  }
}

SourceLocation SourceLocation::Behind(const SourceLocation &loc, u32 dist) {
  ASSERT_GE(loc.cursor.offset, dist);
  SourceLocation result = loc;
  result.cursor.offset -= dist;
  return result;
}
