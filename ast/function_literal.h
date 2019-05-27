#ifndef ICARUS_AST_FUNCTION_LITERAL_H
#define ICARUS_AST_FUNCTION_LITERAL_H

#include <vector>
#include "ast/declaration.h"
#include "ast/expression.h"
#include "base/graph.h"
#include "core/fn_params.h"
#include "core/scope.h"

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

#include "visitor/visitors.xmacro.h"

  std::unique_ptr<core::FnScope> fn_scope_;

  // Note this field is computed, but it is independent of any type or
  // context-specific information. It holds a topologically sorted list of
  // function parameters such that earlier parameters never depend on later
  // ones. It's filled in assign_scope because that's when we have enough
  // information to do so and it guarantees it's only called once.
  //
  // TODO rename assign_scope.
  std::vector<Declaration const *> sorted_params_;
  absl::flat_hash_map<Declaration const *, size_t> decl_to_param_;
  base::Graph<Declaration const *> param_dep_graph_;

  // TODO This is storing both the name in the declaration and pulls the
  // string_view of the name out in core::FnParams::Param.
  core::FnParams<std::unique_ptr<Declaration>> inputs_;
  std::vector<std::unique_ptr<Expression>> outputs_;
  std::vector<std::unique_ptr<Node>> statements_;

  bool return_type_inferred_ = false;
  Module *module_            = nullptr;
};
}  // namespace ast

#endif  // ICARUS_AST_FUNCTION_LITERAL_H
