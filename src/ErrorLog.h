#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <string>
#include <vector>
#include <iostream>

// TODO depending on whether or not we log to the console, to a browser, etc, we
// will want to display things differently. For now, just log to the console.
class ErrorLog {
  public:
    ErrorLog();
    friend std::ostream& operator<<(std::ostream& os, const ErrorLog& log);

    inline size_t num_errors() const;
    inline void set_file(const char* file_name);
    inline void log(size_t line_num, const std::string& msg);

  private:
    std::string file_name_;
    std::vector<std::pair<size_t, std::string>> log_;
};

size_t ErrorLog::num_errors() const { return log_.size(); }

void ErrorLog::set_file(const char* file_name) {
  file_name_ = std::string(file_name);
}

void ErrorLog::log(size_t line_num, const std::string& msg) {
  log_.emplace_back(line_num, msg);
}
#endif  // ICARUS_ERROR_LOG_H
