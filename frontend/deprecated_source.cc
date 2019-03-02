#include "frontend/deprecated_source.h"

#include <iostream>
#include "base/log.h"

namespace frontend {

std::optional<Source::Line> Repl::NextLine() {
  std::cout << (first_entry ? "\n>> " : ".. ");
  first_entry = false;
  std::string input;
  std::getline(std::cin, input);
  input += '\n';
  lines_.push_back(input);
  current_line_ = input;
  return Line(input);
}

std::optional<Source::Line> SrcSource::NextLine() {
  auto chunk = src_.ReadUntil('\n');
  if (chunk.view.empty() && !chunk.more_to_read) { return std::nullopt; }
  return current_line_ = Line(chunk.view);
}

std::vector<std::string> SrcSource::LoadLines() {
  std::vector<std::string> lines{1};

  SrcSource restarted(*FileSrc::Make(src_.path()));
  while (true) {
    auto chunk = restarted.src_.ReadUntil('\n');
    if (chunk.view.empty() && !chunk.more_to_read) { return lines; }
    lines.emplace_back(chunk.view);
  }
}

}  // namespace frontend
