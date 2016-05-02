#ifndef ICARUS_ERROR_LOG_H
#define ICARUS_ERROR_LOG_H

// TODO depending on whether or not we log to the console, to a browser, etc, we
// will want to display things differently. For now, just log to the console.
class ErrorLog {
public:
  ErrorLog();
  friend std::ostream &operator<<(std::ostream &os, const ErrorLog &log);

  size_t num_errors() const;
  void log(TokenLocation loc, const std::string &msg);

  AST::Node *assignment_vs_equality(AST::Node *node);

private:
  size_t err_num_;
  std::map<std::string, std::map<size_t, std::vector<std::string>>> log_;
};

extern ErrorLog error_log;

#endif // ICARUS_ERROR_LOG_H
