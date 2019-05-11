#ifndef ICARUS_AST_STRUCT_TYPE_H
#define ICARUS_AST_STRUCT_TYPE_H

#include "ast/literal.h"

namespace ast {
struct StructType : public Literal {
  StructType(TextSpan span) : Literal(span) {}
  ~StructType() override {}

#include "ast_visitor/visitors.xmacro.h"

  std::string to_string(size_t n) const override;

  ir::Results EmitIr(Context *) override;

  std::vector<std::unique_ptr<Expression>> args_;
};
}  // namespace ast

#endif  // ICARUS_AST_STRUCT_TYPE_H
