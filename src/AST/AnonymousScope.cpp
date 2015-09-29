#include "AST/AnonymousScope.h"

namespace AST {
  std::string AnonymousScope::to_string(size_t n) const {
    std::string output;
    for (size_t i = 0; i < n; ++i) {
      output += "  ";
    }

    output += "<Anonymous Scope>\n";

    return output + statements_->to_string(n + 1);
  }
  void AnonymousScope::verify_no_declarations() const {
    // FIXME need to implement
  }

  std::set<std::string> AnonymousScope::identifiers() const {
    return statements_->identifiers();
  }

  void AnonymousScope::register_declared_variables() {
    // FIXME TODO
  }

  void AnonymousScope::separate_declarations_and_assignments() {
    std::cout << "!!" << std::endl;
    // FIXME TODO
  }
}  // namespace AST
