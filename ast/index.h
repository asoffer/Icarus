#ifndef ICARUS_AST_INDEX_H
#define ICARUS_AST_INDEX_H

#include <memory>
#include <vector>

#include "ast/expression.h"

struct Context;

namespace ast {
struct Index : public Expression {
  ~Index() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;
  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override;
  bool InferType(type::Type const *t, InferenceState *state) const override {
    return false;
  }

  ir::Results EmitIr(Context *) override;
  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  std::unique_ptr<Expression> lhs_, rhs_;
};

}  // namespace ast

#endif  // ICARUS_AST_INDEX_H
