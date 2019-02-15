#ifndef ICARUS_AST_FUNCTION_LITERAL_H
#define ICARUS_AST_FUNCTION_LITERAL_H

#include <unordered_map>
#include <vector>
#include "ast/bound_constants.h"
#include "ast/declaration.h"
#include "ast/dispatch.h"
#include "ast/fn_params.h"
#include "ast/identifier.h"
#include "ast/literal.h"
#include "ast/statements.h"
#include "ir/val.h"
#include "misc/scope.h"

struct Module;

namespace ir {
struct Func;
}  // namespace ir

namespace ast {

struct FunctionLiteral : public Literal {
  // Represents a function with all constants bound to some value.
  FunctionLiteral() {}
  FunctionLiteral(FunctionLiteral &&) noexcept = default;
  ~FunctionLiteral() override {}

  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  VerifyResult VerifyType(Context *) override;
  void Validate(Context *) override;
  void ExtractJumps(JumpExprs *) const override;
  void DependentDecls(base::Graph<Declaration *> *g,
                      Declaration *d) const override;

  VerifyResult VerifyTypeConcrete(Context *);

  std::vector<ir::Val> EmitIR(Context *) override;

  void CompleteBody(Context *ctx);

  std::unique_ptr<FnScope> fn_scope_;

  // Note this field is computed, but it is independent of any type or
  // context-specific information. It holds a topologically sorted list of
  // function parameters such that earlier parameters never depend on later
  // ones. It's filled in assign_scope because that's when we have enough
  // information to do so and it guarantees it's only called once.
  //
  // TODO rename assign_scope.
  std::vector<Declaration *> sorted_params_;
  base::Graph<Declaration *> param_dep_graph_;

  // TODO This is storing both the name in the declaration and pulls the
  // string_view of the name out in FnParams::Param.
  FnParams<std::unique_ptr<Declaration>> inputs_;
  std::vector<std::unique_ptr<Expression>> outputs_;
  Statements statements_;

  bool return_type_inferred_ = false;
  Module *module_            = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_FUNCTION_LITERAL_H