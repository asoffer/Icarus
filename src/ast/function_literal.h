#ifndef ICARUS_AST_FUNCTION_LITERAL_H
#define ICARUS_AST_FUNCTION_LITERAL_H

#include <map>
#include <vector>

#include "ast/bound_constants.h"
#include "ast/declaration.h"
#include "ast/dispatch.h"
#include "ast/expression.h"
#include "ast/identifier.h"
#include "ir/val.h"
#include "scope.h"

namespace IR {
struct Func;
}  // namespace IR

namespace type {
extern Type *Generic;
}  // namespace type

namespace AST {
struct FunctionLiteral : public Expression {
  FunctionLiteral() {}
  FunctionLiteral(FunctionLiteral &&) noexcept = default;
  ~FunctionLiteral() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void ClearIdDecls() override;
  void VerifyType(Context *) override;
  void Validate(Context *) override;
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;

  FunctionLiteral *Clone() const override;

  void CompleteBody(Module *mod);
  IR::Val EmitIR(Context *) override;

  std::unique_ptr<FnScope> fn_scope;

  std::vector<std::unique_ptr<Declaration>> inputs;
  std::vector<std::unique_ptr<Expression>> outputs;
  std::unique_ptr<Statements> statements;

  // Maps the string name of the declared argument to it's index:
  // Example: (a: int, b: char, c: string) -> int
  //           a => 0, b => 1, c => 2
  std::unordered_map<std::string, size_t> lookup_;
  IR::Func *ir_func_                     = nullptr;
  const BoundConstants *bound_constants_ = nullptr;
  bool completed_                        = false;
  bool return_type_inferred_             = true;
};

struct GenericFunctionLiteral : public FunctionLiteral {
  GenericFunctionLiteral() {
    lvalue = Assign::Const;
    type   = type::Generic;
  }

  ~GenericFunctionLiteral() override {}
  std::string to_string(size_t n) const override;
  void assign_scope(Scope *scope) override;
  void ClearIdDecls() override;
  void VerifyType(Context *) override;
  void Validate(Context *) override {};
  void SaveReferences(Scope *scope, std::vector<IR::Val> *args) override;
  void ExtractReturns(std::vector<const Expression *> *) const override;
  void contextualize(
      const Node *correspondant,
      const std::unordered_map<const Expression *, IR::Val> &) override;
  GenericFunctionLiteral *Clone() const override;
  IR::Val EmitIR(Context *) override;

  // Attempts to match the call argument types to the dependent types here. If
  // it can it materializes a function literal and returns a pointer to it.
  // Otherwise, returns nullptr.
  std::pair<FunctionLiteral *, Binding> ComputeType(
      const FnArgs<Expression *> &args, Context *ctx);

  // Holds an ordering of the indices of 'inputs' sorted in such a way that if a
  // type depends on a value of another declaration, the dependent type occurs
  // after the type it depends on.
  std::vector<size_t> decl_order_;

  std::map<BoundConstants, FunctionLiteral> fns_;
};

}  // namespace AST

#endif  // ICARUS_AST_FUNCTION_LITERAL_H
