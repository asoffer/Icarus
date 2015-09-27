#include "AST/Case.h"

namespace AST {
  std::string Case::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }

    output += "<Case>\n";
    output += pairs_->to_string(n);

    return output;
  }

}  // namespace AST
