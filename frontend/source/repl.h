#ifndef ICARUS_FRONTEND_REPL_SOURCE_H
#define ICARUS_FRONTEND_REPL_SOURCE_H

#include "frontend/source/source.h"

namespace frontend {

struct ReplSource : public Source {
  SourceChunk ReadUntil(char delim) override {
    if (view_.empty()) {
      std::cout << "\n>> ";
      std::getline(std::cin, lines_.emplace_back());
      view_ = lines_.back();
    }

    auto pos = view_.find_first_of(delim);
    while (pos == std::string_view::npos) {
      std::cout << "\n.. ";
      // Compute how far `view_` was inside the line, because after we append to
      // the line we need to reset `view_` appropriately.
      int dist = view_.data() - lines_.back().data();
      std::string line;
      std::getline(std::cin, line);
      lines_.back().append(line);
      view_ = lines_.back();
      view_.remove_prefix(dist);
      pos = view_.find_first_of(delim);
    }

    auto result = view_.substr(0, pos);
    view_.remove_prefix(pos + 1);

    return {result, not view_.empty()};
  }

  std::vector<std::string> LoadLines() const override { return lines_; }

  std::vector<std::string> lines_;
  std::string_view view_;
};

}  // namespace frontend

#endif  // ICARUS_FRONTEND_REPL_SOURCE_H
