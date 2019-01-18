#ifndef ICARUS_AST_FUNCTION_LITERAL_H
#define ICARUS_AST_FUNCTION_LITERAL_H

#include "ast/bound_constants.h"
#include "ast/declaration.h"
#include "ast/dispatch.h"
#include "ast/expression.h"
#include "ast/identifier.h"
#include "ast/statements.h"
#include "base/container/map.h"
#include "base/container/vector.h"
#include "ir/val.h"
#include "scope.h"

struct Module;

namespace ir {
struct Func;
}  // namespace ir

namespace ast {

struct FunctionLiteral : public Expression {
  // Represents a function with all constants bound to some value.
  FunctionLiteral() {}
  FunctionLiteral(FunctionLiteral &&) noexcept = default;
  ~FunctionLiteral() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;

  VerifyResult VerifyTypeConcrete(Context *);

  base::vector<ir::Val> EmitIR(Context *) override;
  base::vector<ir::RegisterOr<ir::Addr>> EmitLVal(Context *) override;

  void CompleteBody(Context *ctx);

  std::unique_ptr<FnScope> fn_scope_;

  base::vector<std::unique_ptr<Declaration>> inputs_;
  base::vector<std::unique_ptr<Expression>> outputs_;
  Statements statements_;

  // Maps the string name of the declared argument to it's index:
  // Example: (a: int, b: char, c: string) -> int
  //           a => 0, b => 1, c => 2
  base::unordered_map<std::string, size_t> lookup_;
  bool return_type_inferred_ = false;
  Module *module_            = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_FUNCTION_LITERAL_H
