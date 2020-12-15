#ifndef ICARUS_FRONTEND_STRING_SOURCE_H
#define ICARUS_FRONTEND_STRING_SOURCE_H

#include "frontend/source/buffer.h"
#include "frontend/source/source.h"

namespace frontend {

struct StringSource : public Source {
  ~StringSource() override {}
  StringSource(std::string src) : src_(std::move(src)), view_(src_.chunk(0)) {}

  StringSource(StringSource const& s) : src_(s.src_), view_(src_.chunk(0)) {}
  StringSource(StringSource&& s)
      : src_(std::move(s).src_), view_(src_.chunk(0)) {}

  StringSource& operator=(StringSource const& s) {
    src_  = s.src_;
    view_ = src_.chunk(0);
    return *this;
  }

  StringSource& operator=(StringSource&& s) {
    src_  = std::move(s).src_;
    view_ = src_.chunk(0);
    return *this;
  }

  SourceChunk ReadUntil(char delim) override {
    auto pos = view_.find_first_of(delim);
    if (pos == std::string_view::npos) { return {view_, false}; }
    auto result = view_.substr(0, pos);
    view_.remove_prefix(pos + 1);
    return {result, not view_.empty()};
  }

  std::string_view line(size_t line_num) const override {
    return src_.line(line_num);
  }

  std::string FileName() const override { return "<string>"; }

  SourceBuffer const& buffer() const override { return src_; }

 private:
  SourceBuffer src_;
  std::string_view view_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_STRING_SOURCE_H
