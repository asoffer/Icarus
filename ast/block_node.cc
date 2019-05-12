#include "ast/block_node.h"

#include <sstream>

namespace ast {
std::string BlockNode::to_string(size_t n) const {
  std::stringstream ss;
  ss << name_->to_string(n) << " {\n"
     << stmts_.to_string(n + 1) << std::string(2 * n, ' ') << "} ";
  return ss.str();
}

}  // namespace ast
