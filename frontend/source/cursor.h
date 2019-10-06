#ifndef ICARUS_FRONTEND_SOURCE_CURSOR_H
#define ICARUS_FRONTEND_SOURCE_CURSOR_H

#include <string_view>

#include "base/debug.h"
#include "frontend/source/range.h"

namespace frontend {
struct SourceCursor {
 public:
  explicit constexpr SourceCursor(SourceLoc loc, std::string_view view)
      : loc_(loc), view_(view){};

  // Returns a SourceCursor prefix of the input consisting of the first
  // contiguous collection of characters satisfying the predicate. These
  // characters are removed `*this`.
  template <typename Fn>
  constexpr SourceCursor ConsumeWhile(Fn &&predicate) {
    size_t pos = 0;
    for (char c : view_) {
      if (!predicate(c)) { break; }
      ++pos;
    }
    return remove_prefix(pos);
  }

  SourceCursor remove_prefix(size_t len) {
    ASSERT(len <= view_.size());

    SourceCursor removed(loc_, std::string_view(view_.data(), len));
    view_.remove_prefix(len);
    loc_.offset += len;
    return removed;
  }

  constexpr SourceRange range() const {
    auto end = loc_;
    end.offset += view_.size();
    return SourceRange(loc_, end);
  }

  constexpr SourceLoc loc() const { return loc_; }
  constexpr std::string_view view() const { return view_; }

 private:
  SourceLoc loc_;
  std::string_view view_ = "";
};
}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_CURSOR_H
