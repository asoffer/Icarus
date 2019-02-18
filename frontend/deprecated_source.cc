#include "frontend/deprecated_source.h"
#include <iostream>

namespace frontend {

std::optional<Source::Line> Repl::NextLine() {
  std::cout << (first_entry ? "\n>> " : ".. ");
  first_entry = false;
  std::string input;
  std::getline(std::cin, input);
  input += '\n';
  return Line(input);
}

std::optional<Source::Line> SrcSource::NextLine() {
  auto chunk = src_.ReadUntil('\n');
  if (chunk.view.empty() && !chunk.more_to_read) { return std::nullopt; }
  return Line(chunk.view);
}

}  // namespace frontend
