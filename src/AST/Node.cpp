#include "AST/Node.h"

namespace AST {

  std::ostream& operator<<(std::ostream& os, const Node& node) {
    return os << node.to_string(0);
  }

  std::string Node::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }

    output += "[" + Language::show_name.at(type_);

    if (!token_.empty())
      output += ": " + token_;

    return output + "]\n";
  }

  Node::Node(Language::NodeType type, const std::string& token) : type_(type), token_(token) {
  }

}  // namespace AST
