#include "source.h"

base::optional<Source::Line> File::NextLine() {
  if (ifs.eof()) { return base::none; }
  std::string temp;
  std::getline(ifs, temp);
  return Line(std::move(temp));
}

base::optional<Source::Line> Repl::NextLine() {
  std::cout << (first_entry ? "\n>> " : ".. ");
  first_entry = false;
  std::string input;
  std::getline(std::cin, input);
  input += '\n';
  return Line(input);
}
