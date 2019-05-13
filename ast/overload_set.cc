#include "ast/overload_set.h"

#include "absl/container/flat_hash_set.h"
#include "ast/declaration.h"
#include "misc/context.h"
#include "core/scope.h"

namespace ast {
using ::matcher::InheritsFrom;

// TODO only hold functions?
OverloadSet::OverloadSet(core::Scope *scope, std::string const &id, Context *ctx) {
  auto decls = scope->AllDeclsWithId(id);
  reserve(decls.size());
  for (auto const &decl : decls) {
    if (auto const *result = ctx->prior_verification_attempt(decl)) {
      emplace(decl, *result);
    }
  }
}

void OverloadSet::add_adl(std::string const &id, type::Type const *t) {
  absl::flat_hash_set<::Module const *> modules;
  t->defining_modules(&modules);

  for (auto *mod : modules) {
    ASSIGN_OR(continue, auto &d, mod->GetDecl(id));
    ASSIGN_OR(continue, auto &t, mod->GetType(id));
    // TODO handle this case. I think it's safe to just discard it.
    ASSERT(&t, InheritsFrom<type::Callable>());

    for (auto const &overload : *this) {
      if (&d == overload.expr) { return; }
    }
    // TODO const
    emplace(&d, ast_visitor::VerifyResult{&t, true});
  }
}

}  // namespace ast
