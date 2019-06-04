#include "ast/overload_set.h"

#include "absl/container/flat_hash_set.h"
#include "ast/declaration.h"
#include "ast/node_span.h"
#include "core/scope.h"
#include "misc/context.h"

namespace ast {
using ::matcher::InheritsFrom;

template <typename DeclSpan>
static void EmplaceDecls(OverloadSet *os, DeclSpan &&decls, Context *ctx) {
  os->reserve(decls.size());
  for (auto const *decl : decls) {
    auto const *result_ptr = ctx->prior_verification_attempt(decl);
    if (result_ptr == nullptr) {
      // TODO i'm skeptical this is the right context.
      visitor::VerifyType vis;
      decl->VerifyType(&vis, ctx);
    }
    result_ptr = ctx->prior_verification_attempt(decl);
    if (result_ptr) { os->emplace(decl, *result_ptr); }
  }
}

OverloadSet::OverloadSet(NodeSpan<Declaration const> decls, Context *ctx) {
  EmplaceDecls(this, decls, ctx);
}

// TODO only hold functions?
OverloadSet::OverloadSet(core::Scope *scope, std::string_view id, Context *ctx) {
  EmplaceDecls(this, scope->AllDeclsWithId(id), ctx);
}

void OverloadSet::add_adl(std::string_view id, type::Type const *t) {
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
    emplace(&d, visitor::VerifyResult{&t, true});
  }
}

}  // namespace ast
