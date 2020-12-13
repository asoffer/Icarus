#include <vector>
#include <string>
#include <string_view>

#include "absl/container/inlined_vector.h"
#include "base/debug.h"

namespace frontend {
// Represents source code held in memory. There are two common ways a
// SourceBuffer can be constructed. First, a SourceBuffer can load a static file
// into memory all at once. This is the common case for compilation. Second, a
// buffer may be repeatedly appended to as is the case with a REPL. For this
// reason we keep a container of strings and for each new chunk of source code,
// append it to the container. We use an InlinedVector so that in the common
// case of static compilation, the source data is behind only one indirection
// rather than two. Lines may not span multiple chunks.
struct SourceBuffer {
  explicit SourceBuffer(std::string chunk) { AppendChunk(std::move(chunk)); }

  // Chunks are treated as if all appended chunks represent a single source
  // buffer, but we require that chunks only break at newlines. Thus, an
  // internal invariant of this type is that all chunks except the last one end
  // with a newline. It is therefore a prerequisite of `AppendChunk` that the
  // last chunk inserted has a newline.
  void AppendChunk(std::string chunk) {
    if (not chunks_.empty()) {
      ASSERT(chunks_.back().size() != 0);
      ASSERT(chunks_.back().back() == '\n');
    }
    IndexLineStarts(chunk, chunks_.size());
    chunks_.push_back(std::move(chunk));
  }

  size_t num_chunks() const { return chunks_.size(); }

  // Returns the chunk at the given index (zero-indexed).
  std::string_view chunk(size_t num) const {
    if (num >= chunks_.size()) { return ""; }
    return chunks_[num];
  }

  size_t num_lines() const { return line_start_.size() - 1; }

  // Returns a view of the line with the given index as it would show in most
  // text editors (one-indexed rather than zero-indexed).
  std::string_view line(size_t line_num) const {
    ASSERT(line_num > 0);
    ASSERT(line_num < line_start_.size());
    auto [start_chunk, start_offset] = line_start_[line_num - 1];
    auto [end_chunk, end_offset]     = line_start_[line_num];
    ASSERT(start_chunk < chunks_.size());
    std::string const& line = chunks_[start_chunk];
    ASSERT(start_offset < line.size());
    if (start_chunk == end_chunk) {
      return std::string_view(line.data() + start_offset,
                              end_offset - start_offset);
    } else {
      ASSERT(end_offset == 0);
      return std::string_view(line.data() + start_offset);
    }
  }

 private:
  // Computes and stores the indices for the start location of each line.
  void IndexLineStarts(std::string_view chunk, size_t chunk_index);

  absl::InlinedVector<std::string, 1> chunks_;

  // `IndexedOffset` represents an offset into a given chunk in `chunks_`.
  struct IndexedOffset {
    size_t chunk     = 0;
    size_t offset    = 0;
    auto operator<=>(IndexedOffset const&) const = default;
   };

  // Offsets of lines stored with begin and end sentinels.
   std::vector<IndexedOffset> line_start_ = {
       IndexedOffset{.chunk = 0, .offset = 0}};
};

}  // namespace frontend
