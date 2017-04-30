#include "source.h"

#include "../error_log.h"

std::pair<bool, std::string> File::NextLine() {
  if (ifs.eof()) { return std::make_pair(false, ""); }

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
  return std::make_pair(true, temp);
}

std::pair<bool, std::string> ReplSource::NextLine() {
  if (index_ == lines_.size()) { return std::make_pair(false, ""); }
  return std::make_pair(true, lines_[index_++]);
}

