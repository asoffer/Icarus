#include <map>
#include "Node.h"

namespace AST {

#ifdef DEBUG
std::map<Node::Type, std::string> debug_map = {
  { Node::unknown, "Unknown" },
  { Node::eof, "EOF" },
  { Node::newline, "Newline" },
  { Node::comment, "Comment" },
  { Node::identifier, "Identifier" },
  { Node::integer, "Integer" },
  { Node::real, "Real" },
  { Node::operat, "Operator" },
  { Node::Type::left_paren, "Left Paren" },
  { Node::Type::right_paren, "Right Paren" },
  { Node::Type::left_brace, "Left Brace" },
  { Node::Type::right_brace, "Right Brace" },
  { Node::Type::left_bracket, "Left Bracket" },
  { Node::Type::right_bracket, "Right Bracket" }
};

std::ostream& operator<<(std::ostream& os, const Node& node) {
  return os << "[" << debug_map[node.type_] << ": " << node.token_ << "]";
}
#endif

  Node::Node(Node::Type type, const std::string& token) : type_(type), token_(token) {
  }
}  // namespace AST
