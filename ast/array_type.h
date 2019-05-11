#ifndef ICARUS_AST_ARRAY_TYPE_H
#define ICARUS_AST_ARRAY_TYPE_H

#include "ast/literal.h"

namespace ast {
struct ArrayType : public Literal {
  ~ArrayType() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;
  void DependentDecls(DeclDepGraph *g,
                      Declaration *d) const override;

  bool InferType(type::Type const *t, InferenceState *state) const override;

  ir::Results EmitIr(Context *ctx) override;

  std::unique_ptr<Expression> length_, data_type_;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_TYPE_H
