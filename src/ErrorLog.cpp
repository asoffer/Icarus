#include "ErrorLog.h"

ErrorLog::ErrorLog() {}

std::ostream& operator<<(std::ostream& os, const ErrorLog& log) {
  for (const auto& line_and_msg : log.log_) {
    os << log.file_name_
      << " (line " << line_and_msg.first << "):\n    "
      << line_and_msg.second
      << std::endl << std::endl;
  }
  return os;
}
