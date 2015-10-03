#include "AST/Terminal.h"

namespace AST {
  void Terminal::verify_types() {
  }

  std::string Terminal::to_string(size_t n) const {
    std::string spaces;
    for (size_t i = 0; i < n; ++i) {
      spaces += "  ";
    }
 
    return spaces + "<Terminal (" + expr_type_.to_string() + "): " + token_ + ">\n";
  }
}  // namespace AST
