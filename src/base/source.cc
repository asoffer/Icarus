#include "source.h"

#include "../error_log.h"
#include "../ast/ast.h"

Source::Line File::NextLine() {
  if (ifs.eof()) { Line{"", true}; }

  std::string temp;
  std::getline(ifs, temp);
/*
  // Check for null characters in line
  size_t line_length = temp.size();
  for (size_t i = 0; i < line_length; ++i) {
    if (temp[i] == '\0') {
      temp[i] = ' ';
      ErrorLog::NullCharInSrc(*this);
    } else if (temp[i] < (char)9 ||
               ((char)13 < temp[i] && temp[i] < (char)32) ||
               temp[i] == (char)127) { // Non-graphic characters
      temp[i] = ' ';
      ErrorLog::NonGraphicCharInSrc(*this);
    }
  }
  */
  return Line{std::move(temp), false};
}

Source::Line Repl::NextLine() {
  std::cout << "> ";
  std::string input;
  std::getline(std::cin, input);
  input += '\n';
  return Line{input, false};
}
