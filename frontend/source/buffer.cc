#include <iterator>

#include "frontend/source/buffer.h"

namespace frontend {

base::Interval<LineNum> SourceRange::lines(SourceBuffer const &buffer) const {
  return base::Interval<LineNum>(buffer.line_number(begin()),
                                 buffer.line_number(end()) + 1);
}

void SourceBuffer::AppendChunk(std::string chunk) {
  if (not chunks_.empty()) {
    ASSERT(chunks_.back().size() != 0);
    ASSERT(chunks_.back().back() == '\n');
  }
  IndexLineStarts(chunk, chunks_.size());
  chunks_.push_back(std::move(chunk));
}

void SourceBuffer::IndexLineStarts(std::string_view chunk, size_t chunk_index) {
  auto iter = chunk.begin();
  while (true) {
    iter = std::find(iter, chunk.end(), '\n');
    if (iter == chunk.end()) { break; }
    ++iter;
    if (iter == chunk.end()) { break; }
    line_start_.emplace_back(chunk_index, std::distance(chunk.begin(), iter));
  }

  line_start_.emplace_back(chunk_index + 1, 0);
}

LineNum SourceBuffer::line_number(SourceLoc loc) const {
  auto iter = std::upper_bound(line_start_.begin(), line_start_.end(), loc);
  ASSERT(iter != line_start_.end());
  return LineNum(std::distance(line_start_.begin(), iter));
}

Offset SourceBuffer::offset_in_line(SourceLoc loc) const {
  auto iter = std::upper_bound(line_start_.begin(), line_start_.end(), loc);
  ASSERT(iter != line_start_.end());
  ASSERT(iter != line_start_.begin());
  return Offset(loc.offset_ - std::prev(iter)->offset_);
}

std::pair<LineNum, Offset> SourceBuffer::line_and_offset(SourceLoc loc) const {
  auto iter = std::upper_bound(line_start_.begin(), line_start_.end(), loc);
  ASSERT(iter != line_start_.end());
  return std::pair(LineNum(std::distance(line_start_.begin(), iter)),
                   Offset(loc.offset_ - std::prev(iter)->offset_));
}

SourceLoc SourceBuffer::location(LineNum line_num) const {
  ASSERT(line_num.value <= line_start_.size());
  return line_start_[line_num.value - 1];
}

std::string_view SourceBuffer::operator[](SourceRange const &range) {
  ASSERT(range.begin().chunk_ == range.end().chunk_);
  return std::string_view(chunks_[range.begin().chunk_])
      .substr(range.begin().offset_,
              range.end().offset_ - range.begin().offset_);
}

}  // namespace frontend
