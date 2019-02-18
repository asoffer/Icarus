#include "frontend/deprecated_source.h"
#include <iostream>

namespace frontend {

std::optional<Source::Line> File::NextLine() {
  if (ifs.eof()) { return std::nullopt; }
  std::string temp;
  std::getline(ifs, temp);
  return Line(std::move(temp));
}

std::optional<Source::Line> Repl::NextLine() {
  std::cout << (first_entry ? "\n>> " : ".. ");
  first_entry = false;
  std::string input;
  std::getline(std::cin, input);
  input += '\n';
  return Line(input);
}

}  // namespace frontend
