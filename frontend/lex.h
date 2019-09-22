#ifndef ICARUS_FRONTEND_LEX_H
#define ICARUS_FRONTEND_LEX_H

#include "error/log.h"
#include "frontend/lexeme.h"
#include "frontend/source/source.h"
#include "frontend/source/range.h"

namespace frontend {
extern absl::flat_hash_map<std::string_view, ast::Hashtag::Builtin> const
    BuiltinHashtagMap;

struct SourceCursor {
 public:
  SourceCursor(LineNum line, Offset offset, std::string_view view)
      : line_(line), offset_(offset), view_(view){};

  // Returns a SourceCursor prefix of the input consisting of the first contiguous
  // collection of characters satisfying the predicate. These characters are
  // removed `*this`.
  template <typename Fn>
  SourceCursor ConsumeWhile(Fn &&predicate) {
    size_t pos = 0;
    for (char c : view_) {
      if (!predicate(c)) { break; }
      ++pos;
    }
    return remove_prefix(pos);
  }

  SourceCursor remove_prefix(size_t len) {
    ASSERT(len <= view_.size());

    SourceCursor removed(line_, offset_, std::string_view(view_.data(), len));
    view_.remove_prefix(len);
    offset_ = Offset(offset_.value + len);
    return removed;
  }

  LineNum line() const { return line_; }
  Offset offset() const { return offset_; }
  std::string_view view() const { return view_; }

 private:
  LineNum line_          = LineNum(0);
  Offset offset_         = Offset(0);
  std::string_view view_ = "";
};

struct LexState {
  LexState(Source *src, error::Log *log)
      : src_(src),
        cursor_(LineNum(1), Offset(0), src_->ReadUntil('\n').view),
        error_log_(log) {}

  char peek() {
    ASSERT(cursor_.view().size() != 0u);
    return cursor_.view()[0];
  }

  Source *src_;
  SourceCursor cursor_;
  error::Log *error_log_;
};

Lexeme NextToken(LexState *state);

}  // namespace frontend

#endif  // ICARUS_FRONTEND_LEX_H
