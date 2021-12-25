#ifndef ICARUS_FRONTEND_SOURCE_VIEW_H
#define ICARUS_FRONTEND_SOURCE_VIEW_H

#include "base/debug.h"
#include "frontend/source/buffer.h"

namespace frontend {

struct SourceView {
  explicit SourceView() = default;
  explicit SourceView(SourceBuffer const *buffer, SourceRange const &range)
      : buffer_(buffer), range_(range) {}

  SourceBuffer const &buffer() const { return *ASSERT_NOT_NULL(buffer_); }
  SourceRange const &range() const { return range_; }

  bool operator==(SourceView const &) const = default;
  bool operator!=(SourceView const &) const = default;

 private:
  SourceBuffer const *buffer_ = nullptr;
  SourceRange range_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_SOURCE_VIEW_H
