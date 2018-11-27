#ifndef ICARUS_AST_ARRAY_LITERAL_H
#define ICARUS_AST_ARRAY_LITERAL_H

#include "ast/comma_list.h"

namespace ast {
struct ArrayLiteral : public CommaList {
  ~ArrayLiteral() override {}
  std::string to_string(size_t n) const override;
  type::Type const *VerifyType(Context *) override;

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::Register> EmitLVal(Context *) override;
};
}  // namespace ast

#endif  // ICARUS_AST_ARRAY_LITERAL_H
