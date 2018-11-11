#include "ast/overload_set.h"
#include "ast/declaration.h"
#include "context.h"
#include "scope.h"

namespace ast {
// TODO only hold functions?
OverloadSet::OverloadSet(Scope *scope, std::string const &id, Context *ctx) {
  auto decls = scope->AllDeclsWithId(id, ctx).first;
  reserve(decls.size());
  for (auto const &decl : decls) { emplace_back(decl.get(), decl.type()); }
}
}  // namespace ast
