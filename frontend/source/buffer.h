#ifndef ICARUS_FRONTEND_SOURCE_BUFFER_H
#define ICARUS_FRONTEND_SOURCE_BUFFER_H

#include <string>
#include <string_view>
#include <vector>

#include "absl/container/inlined_vector.h"
#include "base/debug.h"
#include "base/interval.h"
#include "base/strong_types.h"
#include "frontend/source/line.h"

namespace frontend {

struct SourceBuffer;  // Defined below

ICARUS_BASE_DEFINE_STRONG_TYPE(Offset, int32_t{0},  //
                               base::EnableRawArithmetic,
                               base::EnableComparisons);

// Represents a location of source in an implicit SourceBuffer (defined below).
struct SourceLoc {
  explicit constexpr SourceLoc(size_t chunk, size_t offset)
      : chunk_(chunk), offset_(offset) {}

  constexpr SourceLoc &operator+=(Offset offset) {
    offset_ += offset.value;
    return *this;
  }

  constexpr SourceLoc &operator-=(Offset offset) {
    offset_ -= offset.value;
    return *this;
  }

  friend constexpr SourceLoc operator+(SourceLoc loc, Offset offset) {
    return loc += offset;
  }

  friend constexpr SourceLoc operator-(SourceLoc loc, Offset offset) {
    return loc -= offset;
  }

  friend Offset operator-(SourceLoc lhs, SourceLoc rhs) {
    ASSERT(lhs.chunk_ == rhs.chunk_);
    return Offset(lhs.offset_ - rhs.offset_);
  }

  constexpr auto operator<=>(SourceLoc const &) const = default;

  std::string DebugString() const;

 private:
  friend struct SourceBuffer;

  size_t chunk_   = 0;
  int32_t offset_ = 0;
};

// Represents a half-open range of source in an implicit SourceBuffer (defined
// below).
struct SourceRange {
  constexpr SourceRange() : begin_(0, 0), end_(0, 0) {}
  explicit constexpr SourceRange(SourceLoc const &b, SourceLoc const &e)
      : begin_(b), end_(e) {}

  // Constructs a SourceRange. If `n` is positive the range starts at `l` and
  // has length `n`. If `n` is negative, the range ends at `l` and has length
  // `n`. In either case, assumes that no newline characters are present inside
  // the constructed range.
  explicit constexpr SourceRange(SourceLoc const &l, int n)
      : SourceRange(l + Offset{std::min(n, 0)}, l + Offset{std::max(n, 0)}) {}

  base::Interval<LineNum> lines(SourceBuffer const &buffer) const;

  constexpr SourceRange expanded(Offset o) {
    return SourceRange(begin() - o, end() + o);
  }

  constexpr SourceLoc begin() const { return begin_; }
  constexpr SourceLoc end() const { return end_; }

  constexpr SourceLoc &begin() { return begin_; }
  constexpr SourceLoc &end() { return end_; }

  bool operator==(SourceRange const &) const = default;
  bool operator!=(SourceRange const &) const = default;

 private:
  friend struct SourceBuffer;

  SourceLoc begin_, end_;
};

// Represents source code held in memory. There are two common ways a
// SourceBuffer can be constructed. First, a SourceBuffer can load a static file
// into memory all at once. This is the common case for compilation. Second, a
// buffer may be repeatedly appended to as is the case with a REPL. For this
// reason we keep a container of strings and for each new chunk of source code,
// append it to the container. We use an InlinedVector so that in the common
// case of static compilation, the source data is behind only one indirection
// rather than two. Lines may not span multiple chunks.
struct SourceBuffer {
  explicit SourceBuffer(std::string name, std::string chunk)
      : name_(std::move(name)) {
    AppendChunk(std::move(chunk));
  }
  explicit SourceBuffer(std::string chunk)
      : SourceBuffer("", std::move(chunk)) {}

  std::string_view name() const { return name_; }

  // Chunks are treated as if all appended chunks represent a single source
  // buffer, but we require that chunks only break at newlines. Thus, an
  // internal invariant of this type is that all chunks except the last one end
  // with a newline. It is therefore a prerequisite of `AppendChunk` that the
  // last chunk inserted has a newline.
  void AppendChunk(std::string chunk);

  size_t num_chunks() const { return chunks_.size(); }

  // Returns the chunk at the given index (zero-indexed). The returned
  // string_view is valid for the lifetime of this SourceBuffer.
  std::string_view chunk(size_t num) const {
    if (num >= chunks_.size()) { return ""; }
    return chunks_[num];
  }

  // The returned string_view is valid for the lifetime of this SourceBuffer.
  std::string_view last_chunk() const { return chunks_.back(); }

  size_t num_lines() const { return line_start_.size() - 1; }

  // Returns a view of the line with the given index as it would show in most
  // text editors (one-indexed rather than zero-indexed).  The returned
  // string_view is valid for the lifetime of this SourceBuffer.
  std::string_view line(LineNum l) const {
    size_t line_num = l.value;
    ASSERT(line_num > 0);
    ASSERT(line_num < line_start_.size());
    auto [start_chunk, start_offset] = line_start_[line_num - 1];
    auto [end_chunk, end_offset]     = line_start_[line_num];
    ASSERT(start_chunk < chunks_.size());
    std::string const &line = chunks_[start_chunk];
    ASSERT(start_offset < line.size());
    if (start_chunk == end_chunk) {
      return std::string_view(line.data() + start_offset,
                              end_offset - start_offset);
    } else {
      ASSERT(end_offset == 0);
      return std::string_view(line.data() + start_offset);
    }
  }

  // Returns the line number of the line containing this source location.
  LineNum line_number(SourceLoc loc) const;

  // Returns the offset into this line for the given source location.
  Offset offset_in_line(SourceLoc loc) const;

  // Returns the line number and offset of the source location into this buffer.
  std::pair<LineNum, Offset> line_and_offset(SourceLoc loc) const;

  // Returns the source location representing the start of the given line
  // number.
  SourceLoc location(LineNum line_num) const;

  // Returns a string_view of the source code in this buffer in the given range.
  std::string_view operator[](SourceRange const &range) const;

  // Returns the character at the given source location.
  char operator[](SourceLoc loc) const {
    return chunks_[loc.chunk_][loc.offset_];
  }

  // Starting at `loc`, finds the next sequence of characters satisfying the
  // predicate `pred` and returns the pair of the corresponding SourceRange and
  // a std::string_view representing that source text. The in-out parameter
  // `loc` is updated to be the next location at which `pred` no longer holds.
  // Each predicate must return false at some point between `loc` and the end of
  // the chunk referenced by `loc` (behavior is undefined otherwise).
  template <std::predicate<char> P>
  std::pair<SourceRange, std::string_view> ConsumeChunkWhile(SourceLoc &loc,
                                                             P &&pred) const {
    SourceLoc start_loc = loc;
    ASSERT(loc.chunk_ < chunks_.size());
    std::string_view chunk = chunks_[loc.chunk_];
    size_t offset          = loc.offset_;
    while (pred(chunk[offset])) {
      ++offset;
      ASSERT(offset < chunk.size());
    }
    std::string_view result = chunk.substr(loc.offset_, offset - loc.offset_);
    loc.offset_             = offset;
    return std::make_pair(SourceRange(start_loc, loc), result);
  }

  // Returns a SourceLoc referring to one character passed the end of the source
  // buffer.
  SourceLoc end() const { return line_start_.back(); }

 private:
  // Computes and stores the indices for the start location of each line.
  void IndexLineStarts(std::string_view chunk, size_t chunk_index);

  absl::InlinedVector<std::string, 1> chunks_;

  // Offsets of lines stored with begin and end sentinels.
  std::vector<SourceLoc> line_start_ = {SourceLoc(0, 0)};
  std::string name_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_BUFFER_H
