#ifndef ICARUS_AST_STATEMENTS_H
#define ICARUS_AST_STATEMENTS_H

#include "typedefs.h"
#include "AST/Node.h"
#include "AST/Expression.h"

namespace AST {
  class Statements : public Node {
    public:
      static NPtr build_one(NPtrVec&& nodes);
      static NPtr build_more(NPtrVec&& nodes);

      virtual std::string to_string(size_t n) const;

    private:
      Statements() {}
      std::vector<EPtr> statements_;
  };

  inline NPtr Statements::build_one(NPtrVec&& nodes) {
    auto output = new Statements;
    output->statements_.emplace_back(static_cast<Expression*>(nodes[0].release()));

    return NPtr(output);
  }

  inline NPtr Statements::build_more(NPtrVec&& nodes) {
    auto output = static_cast<Statements*>(nodes[0].release());
    output->statements_.emplace_back(static_cast<Expression*>(nodes[1].release()));

    return NPtr(output);
  }

}  // namespace AST

#endif  // ICARUS_AST_STATEMENTS_H


