#include "ast/overload_set.h"

#include "absl/container/flat_hash_set.h"
#include "ast/ast.h"
#include "ast/scope/scope.h"

namespace ast {

OverloadSet::OverloadSet(absl::Span<Declaration const *const> decls) {
  reserve(decls.size());
  for (auto const *decl : decls) { insert(decl); }
}

OverloadSet::OverloadSet(ast::Scope const *scope, std::string_view id)
    : OverloadSet(scope->AllDeclsWithId(id)) {}

}  // namespace ast
