#ifndef ICARUS_IDENTIFIER_H
#define ICARUS_IDENTIFIER_H

#include <map>
#include "AST/Node.h"
#include "AST/Terminal.h"
#include "Language.h"

namespace AST {
  class Identifier : public Terminal {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;
      virtual std::set<std::string> identifiers() const;

    private:
      Identifier() {}
  };

  inline NPtr Identifier::build(NPtrVec&& nodes) {
    auto id_ptr = new Identifier;
    id_ptr->base_type_ = Language::identifier;
    id_ptr->token_ = nodes[0]->token();
    id_ptr->precedence_ = Language::op_prec.at("MAX");

    return NPtr(id_ptr);
  }

  inline std::string Identifier::to_string(size_t n) const {
    std::string spaces;
    for (size_t i = 0; i < n; ++i) {
      spaces += "  ";
    }
 
    return spaces + "<Identifier: " + token() + ">\n";
  }

  inline std::set<std::string> Identifier::identifiers() const {
    return { token() };
  }

}  // namespace AST
#endif  // ICARUS_IDENTIFIER_H
