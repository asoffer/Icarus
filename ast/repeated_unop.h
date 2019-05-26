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

  frontend::Operator op_;
  CommaList args_;
};
}  // namespace ast
#endif  // ICARUS_AST_REPEATED_UNOP_H
