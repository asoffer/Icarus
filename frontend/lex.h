#ifndef ICARUS_FRONTEND_LEX_H
#define ICARUS_FRONTEND_LEX_H

#include "error/log.h"
#include "frontend/lexeme.h"
#include "frontend/source.h"
#include "frontend/text_span.h"

namespace frontend {

struct SrcCursor {
 public:
  SrcCursor(int line, int offset, std::string_view view)
      : line_(line), offset_(offset), view_(view){};

  // Returns a SrcCursor prefix of the input consisting of the first contiguous
  // collection of characters satisfying the predicate. These characters are
  // removed `*this`.
  template <typename Fn>
  SrcCursor ConsumeWhile(Fn &&predicate) {
    size_t pos = 0;
    for (char c : view_) {
      if (!predicate(c)) { break; }
      ++pos;
    }
    return remove_prefix(pos);
  }

  SrcCursor remove_prefix(size_t len) {
    ASSERT(len <= view_.size());

    SrcCursor removed{line_, offset_, std::string_view{view_.data(), len}};
    view_.remove_prefix(len);
    offset_ += len;
    return removed;
  }

  int line() const { return line_; }
  int offset() const { return offset_; }
  std::string_view view() const { return view_; }

 private:

  int line_   = 0;
  int offset_ = 0;
  std::string_view view_;
};

struct LexState {
  LexState(Src *src, error::Log *log)
      : src_(src),
        cursor_(1, 0, src_->ReadUntil('\n').view),
        error_log_(log) {}

  char peek() {
    ASSERT(cursor_.view().size() != 0u);
    return cursor_.view()[0];
  }

  Src *src_;
  SrcCursor cursor_;
  error::Log *error_log_;
};

Lexeme NextToken(LexState *state);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_H
