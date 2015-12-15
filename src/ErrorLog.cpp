#include "ErrorLog.h"

#include "Language.h"
#include "AST.h"

ErrorLog::ErrorLog() : err_num_(0) {}

NPtr ErrorLog::assignment_vs_equality(NPtr node) {
  log(node->line_num(), "Assignment found where boolean expression was expected. Did you mean `==` instead of `=`?");

  auto assignment_node = std::static_pointer_cast<AST::Binop>(node);

  NPtrVec node_vec = {
    std::static_pointer_cast<AST::Node>(assignment_node->lhs_),
    NPtr(new AST::Node(node->line_num(), Language::generic_operator, "==")),
    std::static_pointer_cast<AST::Node>(assignment_node->rhs_)
  };

  return AST::ChainOp::join(std::forward<NPtrVec>(node_vec));
}

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
