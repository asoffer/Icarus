#include "ast/overload_set.h"
#include "ast/declaration.h"
#include "context.h"
#include "scope.h"

namespace AST {
OverloadSet::OverloadSet(Scope *scope, std::string const &id, Context *ctx) {
  auto decls = scope->AllDeclsWithId(id, ctx).first;
  reserve(decls.size());
  for (auto const &decl : decls) { emplace_back(decl); }
}
}  // namespace AST
