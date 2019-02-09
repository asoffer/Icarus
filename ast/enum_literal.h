#ifndef ICARUS_AST_ENUM_LITERAL_H
#define ICARUS_AST_ENUM_LITERAL_H

#include <string>

#include "ast/declaration.h"
#include <vector>
#include "misc/scope.h"

namespace ast {
// TODO rename this because it's for enums and flags.
struct EnumLiteral : public Expression {
  enum Kind : char { Enum, Flags };

  EnumLiteral(std::vector<std::unique_ptr<Expression>> elems, TextSpan span,
              bool is_enum)
      : Expression(std::move(span)),
        elems_(std::move(elems)),
        kind_(is_enum ? Kind::Enum : Kind::Flags) {}

  ~EnumLiteral() override {}
  std::string to_string(size_t n) const override;

  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;

  void ExtractJumps(JumpExprs *rets) const override;

  std::vector<ir::Val> EmitIR(Context *) override;
  std::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override {
    UNREACHABLE();
  }

  std::unique_ptr<DeclScope> enum_scope_;
  std::vector<std::unique_ptr<Expression>> elems_;
  Kind kind_;
};

}  // namespace ast

#endif  // ICARUS_AST_ENUM_LITERAL_H
