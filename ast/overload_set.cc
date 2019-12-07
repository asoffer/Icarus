#include "ast/overload_set.h"

#include "absl/container/flat_hash_set.h"
#include "ast/ast.h"
#include "ast/scope/scope.h"

namespace ast {

OverloadSet::OverloadSet(absl::Span<Declaration const *const> decls) {
  members_.reserve(decls.size());
  for (auto const *decl : decls) { members_.insert(decl); }
}

OverloadSet::OverloadSet(ast::Scope const *scope, std::string_view id)
    : OverloadSet(scope->AllDeclsTowardsRoot(id)) {
  DEBUG_LOG("OverloadSet")("Constructing an overload set from \"", id, "\"");
}

}  // namespace ast
