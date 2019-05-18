#ifndef ICARUS_AST_REPEATED_UNOP_H
#define ICARUS_AST_REPEATED_UNOP_H

#include "ast/comma_list.h"
#include "ast/node.h"
#include "frontend/operators.h"

namespace ast {
struct RepeatedUnop : public Node {
  RepeatedUnop(TextSpan const &span) : Node(span) { args_.span = span; }
  ~RepeatedUnop() override {}

#include "visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override {
    switch (op_) {
      case frontend::Operator::Jump: return "jump " + args_.to_string(n);
      case frontend::Operator::Return: return "return " + args_.to_string(n);
      case frontend::Operator::Yield: return "yield " + args_.to_string(n);
      case frontend::Operator::Print: return "print " + args_.to_string(n);
      default: { UNREACHABLE(); }
    }
  }

  frontend::Operator op_;
  CommaList args_;
};
}  // namespace ast
#endif  // ICARUS_AST_REPEATED_UNOP_H
