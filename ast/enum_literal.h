#ifndef ICARUS_AST_ENUM_LITERAL_H
#define ICARUS_AST_ENUM_LITERAL_H

#include <string>
#include <vector>

#include "ast/declaration.h"
#include "ast/literal.h"
#include "misc/scope.h"

namespace ast {
// TODO rename this because it's for enums and flags.
struct EnumLiteral : public Literal {
  enum Kind : char { Enum, Flags };

  EnumLiteral(std::vector<std::unique_ptr<Expression>> elems, TextSpan span,
              bool is_enum)
      : Literal(std::move(span)),
        elems_(std::move(elems)),
        kind_(is_enum ? Kind::Enum : Kind::Flags) {}

  ~EnumLiteral() override {}
  std::string to_string(size_t n) const override;

  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;

  void ExtractJumps(JumpExprs *rets) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  ir::Results EmitIr(Context *) override;

  std::unique_ptr<DeclScope> enum_scope_;
  std::vector<std::unique_ptr<Expression>> elems_;
  Kind kind_;
};

}  // namespace ast

#endif  // ICARUS_AST_ENUM_LITERAL_H
