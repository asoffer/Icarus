#ifndef ICARUS_SEMANTIC_ANALYSIS_CONTEXT_H
#define ICARUS_SEMANTIC_ANALYSIS_CONTEXT_H

#include "ast/ast.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

// `Context` is responsible for holding all information computed during the
// compilation process of a single module.
struct Context {
  // Returns the qualified types associated with `expr`. Requires that `expr`
  // already have some qualified types associated with it on this `Context`.
  absl::Span<QualifiedType const> qualified_types(
      ast::Expression const *expr) const;

  // Returns the qualified type associated with `expr`. Requires that `expr`
  // already have exactly one qualified types associated with it on this
  // `Context`.
  QualifiedType qualified_type(ast::Expression const *expr) const;

  // Sets the qualified types associated with `expr` to be `qualified_types`.
  // Requires that `expr` does not yet have any qualified types associated with
  // it on this `Context.`
  absl::Span<QualifiedType const> set_qualified_types(
      ast::Expression const *expr, std::vector<QualifiedType> qualified_types);

  // Sets the qualified types associated with `expr` to be a sequence consisting
  // of just the one value `qualified_type`. Requires that `expr` does not yet
  // have any qualified types associated with it on this `Context.`
  absl::Span<QualifiedType const> set_qualified_type(
      ast::Expression const *expr, QualifiedType qualified_types);

  // A data structure responsible for identifying a callable AST node along with
  // all information required to complete its type-verification. During
  // verification of a call-expression, we only verify the parameters of callees
  // before considering viable overloads. This allows us to address situations
  // like the one in this example:
  //
  // ```
  // f ::= (x: i64, y: bool) => f(y, x)
  // f ::= (x: bool, y: i64) => true
  // ```
  //
  // When computing the type of `f` we are in the middle of the type-checking
  // process for a function in the overload set of `f`. In particular, the
  // computation is relevant to the type of that overload. It is not possible to
  // completely evaluate the types of all overloads in `f` before proceeding
  // further. It is however possible to evaluate the types of all parameters.
  // Because parameters (and not return types) are all that can be used during
  // type-verification of call-expressions, this is sufficient to see that the
  // overload referred to in `f(y, x)` must be the second one (and not
  // self-referentially the first one). Once this overload is resolved we need
  // to continue the process of computing the type of the chosen overload.
  struct CallableIdentifier {
    explicit CallableIdentifier(ast::Expression const *expression)
        : expression_(expression) {}

    ast::Expression const &expression() const { return *expression_; }

   private:
    ast::Expression const *expression_;
  };

  // Sets the parameter types associated with a `ast::ParameterizedExpression`
  // node that can be used to invoke the expression.
  absl::Span<absl::flat_hash_map<core::ParameterType,
                                 Context::CallableIdentifier> const>
  set_parameters(
      ast::Expression const *expr,
      std::vector<absl::flat_hash_map<core::ParameterType, CallableIdentifier>>
          parameters);

 private:
  absl::flat_hash_map<ast::Expression const *, std::vector<QualifiedType>>
      type_;

  absl::flat_hash_map<
      ast::Expression const *,
      std::vector<absl::flat_hash_map<core::ParameterType, CallableIdentifier>>>
      parameters_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_CONTEXT_H
