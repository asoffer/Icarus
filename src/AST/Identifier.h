#ifndef ICARUS_IDENTIFIER_H
#define ICARUS_IDENTIFIER_H

#include "AST/Node.h"
#include "AST/Terminal.h"
#include "Language.h"

namespace AST {
  class Identifier : public Terminal {
    public:
      static NPtr build(NPtrVec&& nodes);

      virtual bool is_identifier() const { return true; }

      virtual std::string to_string(size_t n) const;

      Identifier(const std::string& token_string) {
        token_ = token_string;
        expr_type_ = Type::Unknown;
        precedence_ = Language::op_prec.at("MAX");
      }
  };

  inline NPtr Identifier::build(NPtrVec&& nodes) {
    return NPtr(new Identifier(nodes[0]->token()));
  }

  inline std::string Identifier::to_string(size_t n) const {
    std::string spaces;
    for (size_t i = 0; i < n; ++i) {
      spaces += "  ";
    }
 
    return spaces + "<Identifier (" + expr_type_.to_string() + "): " + token() + ">\n";
  }

}  // namespace AST
#endif  // ICARUS_IDENTIFIER_H
