#include "ErrorLog.h"

// The global error log
ErrorLog error_log;

ErrorLog::ErrorLog() : err_num_(0) {}

AST::Node *ErrorLog::assignment_vs_equality(AST::Node *node) {
  log(node->loc, "Assignment found where boolean expression was expected. "
                 "Did you mean `==` instead of `=`?");

  auto assignment_node = (AST::Binop *)node;

  NPtrVec node_vec = {
      assignment_node->lhs,
      new AST::TokenNode(node->loc, Language::op_b, "=="),
      assignment_node->rhs};

  return AST::ChainOp::join(std::forward<NPtrVec>(node_vec));
}

std::ostream &operator<<(std::ostream &os, const ErrorLog &log) {

  for (const auto &file_log : log.log_) {
    //NOTE: No sense in robustifying this. It probably won't last.
    size_t num_eqs = 38 - file_log.first.size() / 2;
    std::string eqs(num_eqs, '=');
    os << eqs << ' ' << file_log.first << ' ' << eqs << std::endl;

    for (const auto &line_log : file_log.second) {
      os << "  (line " << line_log.first << "):\n";

      for (const auto &msg : line_log.second) { os << "    " << msg << "\n\n"; }
    }
  }

  os << log.err_num_ << " error";
  if (log.err_num_ != 1) { os << "s"; }

  return os << " found." << std::endl;
}

size_t ErrorLog::num_errors() const { return log_.size(); }

void ErrorLog::log(TokenLocation loc, const std::string &msg) {
  ++err_num_;
  log_[loc.file][loc.line_num].push_back(msg);
}
