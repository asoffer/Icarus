#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

#include <string>
#include <map>
#include <vector>
#include <iostream>

#include "typedefs.h"

// TODO depending on whether or not we log to the console, to a browser, etc, we
// will want to display things differently. For now, just log to the console.
class ErrorLog {
  public:
    ErrorLog();
    friend std::ostream& operator<<(std::ostream& os, const ErrorLog& log);

    inline size_t num_errors() const;
    inline void set_file(const std::string& file_name);
    inline void log(size_t line_num, const std::string& msg);

    AST::Node *assignment_vs_equality(AST::Node *node);

  private:
    size_t err_num_;
    std::string file_name_;
    std::map<size_t, std::vector<std::string>> log_;
};

size_t ErrorLog::num_errors() const { return log_.size(); }

void ErrorLog::set_file(const std::string& file_name) {
  file_name_ = file_name;
}

void ErrorLog::log(size_t line_num, const std::string& msg) {
  ++err_num_;
  log_[line_num].push_back(msg);
}

#endif  // ICARUS_ERROR_LOG_H
