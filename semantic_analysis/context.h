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

  // Sets the qualified types associated with `expr` to be `qualified_types`.
  // Requires that `expr` does not yet have any qualified types associated with
  // it on this `Context.`
  void set_qualified_types(ast::Expression const *expr,
                           std::vector<QualifiedType> qualified_types);

  // Sets the qualified types associated with `expr` to be a sequence consisting
  // of just the one value `qualified_type`. Requires that `expr` does not yet
  // have any qualified types associated with it on this `Context.`
  void set_qualified_type(ast::Expression const *expr,
                          QualifiedType qualified_types);

 private:
  absl::flat_hash_map<ast::Expression const *, std::vector<QualifiedType>>
      type_;
};

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_CONTEXT_H
