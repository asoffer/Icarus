#include "ErrorLog.h"

ErrorLog::ErrorLog() : err_num_(0) {}

AST::Node *ErrorLog::assignment_vs_equality(AST::Node *node) {
  log(node->line_num, "Assignment found where boolean expression was expected. "
                      "Did you mean `==` instead of `=`?");

  auto assignment_node = (AST::Binop *)node;

  NPtrVec node_vec = {
      assignment_node->lhs,
      new AST::TokenNode(node->line_num, Language::generic_operator, "=="),
      assignment_node->rhs};

  return AST::ChainOp::join(std::forward<NPtrVec>(node_vec));
}

std::ostream &operator<<(std::ostream &os, const ErrorLog &log) {
  for (const auto &line_and_msgs : log.log_) {
    os << log.file_name_ << " (line " << line_and_msgs.first << "):\n";
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
