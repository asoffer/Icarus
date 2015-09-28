#include "AST/Terminal.h"

namespace AST {
  std::string Terminal::to_string(size_t n) const {
    std::string spaces;
    for (size_t i = 0; i < n; ++i) {
      spaces += "  ";
    }
 
    return spaces + "<" + Node::debug_map[base_type_] + ": " + token_ + ">\n";
  }
}  // namespace AST
