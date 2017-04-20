#include "cursor.h"
#include "error_log.h"
#include "base/debug.h"

void Cursor::MoveToNextLine() {
  ASSERT(source_file, "");
  ASSERT(!source_file->ifs.eof(), "");
  std::string temp;
  std::getline(source_file->ifs, temp);

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

  offset = 0;
  line = pstr(temp.c_str());

  ++line_num;
  source_file->lines.push_back(line);
}

Cursor Cursor::Behind(const Cursor &cursor, u64 dist) {
  ASSERT(cursor.offset >= dist, "");
  Cursor result = cursor;
  result.offset -= dist;
  return result;
}
