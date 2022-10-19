#ifndef ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_DIAGNOSTICS_H
#define ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_DIAGNOSTICS_H

#include <string>

#include "ast/expression.h"
#include "semantic_analysis/context.h"
#include "semantic_analysis/type_system.h"

namespace semantic_analysis {

// Returns a human readable string representation of the type of the expression
// suitable for use in diagnostics. The choice of string is intended to be the
// most relevant for the given situation. For example,
//
// ```
// Int ::= i64
// n: Int
// m := n
// ```
//
// When referring to the type of `m` in a diagnostic, TypeForDiagnostic should
// return "Int" rather than "i64", because the type was inferred from the type
// of `n` which was defined with `Int` rather than `i64`.
//
// Requires that `expr` have been type-checked without producing any fatal
// errors before being called.
std::string TypeForDiagnostic(ast::Expression const& expr,
                              Context const& context, TypeSystem& type_system);

// Returns a human readable string representation of the expression suitable for
// use in diagnostics. The choice of string is intended to be the most relevant
// for the given situation.
//
// Requires that `expr` have been type-checked without producing any fatal
// errors before being called.
std::string ExpressionForDiagnostic(ast::Expression const& expr,
                                    Context const& context,
                                    TypeSystem& type_system);

}  // namespace semantic_analysis

#endif  // ICARUS_SEMANTIC_ANALYSIS_TYPE_VERIFICATION_DIAGNOSTICS_H
