#ifndef ICARUS_REPL_SOURCE_H
#define ICARUS_REPL_SOURCE_H

#include <iostream>
#include <string_view>
#include <vector>

#include "frontend/source/source.h"

namespace repl {

struct Source : frontend::Source {
  Source(std::istream* input, std::ostream* output)
      : input_(*ASSERT_NOT_NULL(input)), output_(*ASSERT_NOT_NULL(output)) {}
  ~Source() override {}

  frontend::SourceChunk ReadUntil(char delim) override {
    if (view_.empty()) {
      output_ << "\n>> ";
      std::getline(input_, lines_.emplace_back());
      lines_.back().push_back('\n');
      view_ = lines_.back();
    }

    auto pos = view_.find_first_of(delim);
    while (pos == std::string_view::npos) {
      output_ << "\n.. ";
      // Compute how far `view_` was inside the line, because after we append to
      // the line we need to reset `view_` appropriately.
      int dist = view_.data() - lines_.back().data();
      std::string line;
      std::getline(input_, line);
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

  std::istream& input_;
  std::ostream& output_;
  std::vector<std::string> lines_;
  std::string_view view_;
};

}  // namespace repl

#endif  // ICARUS_REPL_SOURCE_H
