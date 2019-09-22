#ifndef ICARUS_FRONTEND_STRING_SOURCE_H
#define ICARUS_FRONTEND_STRING_SOURCE_H

#include "frontend/source/source.h"

namespace frontend {

struct StringSource : public Source {
  ~StringSource() override {}
  StringSource(std::string src) : src_(std::move(src)), view_(src_) {}

  SourceChunk ReadUntil(char delim) override {
    auto pos = view_.find_first_of(delim);
    if (pos == std::string_view::npos) { return {view_, false}; }
    auto result = view_.substr(0, pos);
    view_.remove_prefix(pos + 1);
    return {result, !view_.empty()};
  }

  std::vector<std::string> LoadLines() override {
    std::vector<std::string> lines{1};

    std::string_view all{src_};
    auto pos = all.find_first_of('\n');
    while (pos != std::string_view::npos) {
      lines.push_back(std::string{all.substr(0, pos)});
      all.remove_prefix(pos + 1);
      pos = all.find_first_of('\n');
    }
    lines.push_back(std::string{all});

    return lines;
  }

 private:
  std::string src_;
  std::string_view view_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_STRING_SOURCE_H
