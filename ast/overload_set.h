#ifndef ICARUS_AST_OVERLOAD_SET_H
#define ICARUS_AST_OVERLOAD_SET_H

#include <string_view>
#include <utility>

#include "ast/ast.h"
#include "ast/scope/scope.h"
#include "base/bag.h"

namespace ast {

// Represents a collection of callable expressions all passed around as a single
// entity. The simplest way for one to arise in practice in a language is by
// defining two functions with the same name (but different types:
//
// ```
// f ::= (n: int32) -> int32 { ... }
// f ::= (b: bool) -> bool { ... }
// ```
//
// TODO consider other overload set constructions, like explicitly combining
// other functions or overload sets with some operator.
//
// TODO unit tests
// TODO remove the inheritance. Make the bag a member.
struct OverloadSet : public base::bag<Expression const *> {
  OverloadSet() = default;

  // Construct an overlaod set from a collection of declarations.
  OverloadSet(absl::Span<Declaration const *const> decls);

  // Construct an overload set from all declarations visibile in `scope` that
  // have the name `id`.
  OverloadSet(Scope const *scope, std::string_view id);
};

}  // namespace ast

#endif  // ICARUS_AST_OVERLOAD_SET_H
