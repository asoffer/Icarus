#ifndef ICARUS_AST_EXPRESSION_H
#define ICARUS_AST_EXPRESSION_H

#include <string>
#include <iostream>
#include "typedefs.h"
#include "AST/Node.h"

namespace AST {
  class Expression : public Node {
    public:
      static constexpr size_t prec_terminal = 100;

      static NPtr from_identifier(std::vector<NPtr>&& nodes);
    private:
      Expression() = delete;
      Expression(Node::Type type, const std::string token, size_t prec) : Node(type, token), precedence_(prec) {}
      size_t precedence_;
  };

  inline NPtr Expression::from_identifier(std::vector<NPtr>&& nodes) {
    return std::unique_ptr<Node>(new Expression(
          expression,
          nodes[0]->token_,
          prec_terminal));
  }
}  // namespace AST

#endif  // ICARUS_AST_EXPRESSION_H
