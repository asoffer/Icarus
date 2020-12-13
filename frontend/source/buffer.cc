#include <iostream>
#include <iterator>

#include "frontend/source/buffer.h"

namespace frontend {

void SourceBuffer::IndexLineStarts(std::string_view chunk, size_t chunk_index) {
  auto iter = chunk.begin();
  while (true) {
    iter = std::find(iter, chunk.end(), '\n');
    if (iter == chunk.end()) { break; }
    ++iter;
    line_start_.push_back(IndexedOffset{
        .chunk  = chunk_index,
        .offset = static_cast<size_t>(std::distance(chunk.begin(), iter)),
    });
  }

  line_start_.back() = IndexedOffset{
      .chunk  = chunk_index + 1,
      .offset = 0,
  };
}

}  // namespace frontend
