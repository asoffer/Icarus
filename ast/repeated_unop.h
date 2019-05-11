#ifndef ICARUS_AST_REPEATED_UNOP_H
#define ICARUS_AST_REPEATED_UNOP_H

#include "ast/comma_list.h"
#include "ast/node.h"
#include "frontend/operators.h"

namespace ast {
struct RepeatedUnop : public Node {
  RepeatedUnop(TextSpan const &span);
  ~RepeatedUnop() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  ir::Results EmitIr(Context *) override;

  frontend::Operator op_;
  CommaList args_;
};
}  // namespace ast
#endif  // ICARUS_AST_REPEATED_UNOP_H
