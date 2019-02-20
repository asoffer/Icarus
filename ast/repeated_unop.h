#ifndef ICARUS_AST_REPEATED_UNOP_H
#define ICARUS_AST_REPEATED_UNOP_H

#include "ast/comma_list.h"
#include "ast/dispatch.h"
#include "ast/node.h"
#include "frontend/operators.h"

namespace ast {
struct RepeatedUnop : public Node {
  RepeatedUnop(TextSpan const &span);
  ~RepeatedUnop() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  std::vector<ir::Val> EmitIR(Context *) override;

  frontend::Operator op_;
  CommaList args_;
};
}  // namespace ast
#endif  // ICARUS_AST_REPEATED_UNOP_H
