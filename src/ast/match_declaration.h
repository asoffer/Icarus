#ifndef ICARUS_AST_MATCH_DECLARATION_H
#define ICARUS_AST_MATCH_DECLARATION_H

#include "ast/declaration.h"

namespace AST {
struct MatchDeclaration : public Declaration {
  MatchDeclaration() {}
  MatchDeclaration(MatchDeclaration &&) noexcept = default;
  MatchDeclaration &operator=(MatchDeclaration &&) noexcept = default;
  ~MatchDeclaration() override {}

  std::string to_string(size_t n) const override;
  type::Type const *VerifyType(Context *) override;
  void Validate(Context *) override;

  MatchDeclaration *Clone() const override;
  base::vector<IR::Val> EmitIR(Context *) override;
};
}  // namespace AST

#endif  // ICARUS_AST_MATCH_DECLARATION_H
