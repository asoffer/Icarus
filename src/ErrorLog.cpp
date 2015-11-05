#include "ErrorLog.h"

ErrorLog::ErrorLog() : err_num_(0) {}

std::ostream& operator<<(std::ostream& os, const ErrorLog& log) {
  for (const auto& line_and_msgs : log.log_) {
    os << log.file_name_
      << " (line " << line_and_msgs.first << "):\n";
    for (const auto msg : line_and_msgs.second) {
      os << "    " << msg << "\n\n";
    }
  }

  os << log.err_num_ << " error";
  if (log.err_num_ != 1) {
    os << "s";
  }

  return os << " found." << std::endl;
}
