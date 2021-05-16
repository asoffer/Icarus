#ifndef ICARUS_COMPILER_TYPE_FOR_DIAGNOSTIC_H
#define ICARUS_COMPILER_TYPE_FOR_DIAGNOSTIC_H

#include <string>

#include "ast/expression.h"
#include "compiler/context.h"

namespace compiler {

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
std::string TypeForDiagnostic(ast::Expression const* expr,
                              Context const& context);

}  // namespace compiler

#endif  // ICARUS_COMPILER_TYPE_FOR_DIAGNOSTIC_H
