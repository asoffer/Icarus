#include "ast/overload_set.h"

#include "absl/container/flat_hash_set.h"
#include "ast/ast.h"
#include "base/ptr_span.h"
#include "core/scope.h"

namespace ast {

template <typename DeclSpan>
static void EmplaceDecls(OverloadSet *os, DeclSpan &&decls,
                         compiler::Compiler *visitor) {
  os->reserve(decls.size());
  for (auto const *decl : decls) {
    auto const *result_ptr = visitor->prior_verification_attempt(decl);
    if (!result_ptr) { decl->VerifyType(visitor); }
    result_ptr = visitor->prior_verification_attempt(decl);
    if (result_ptr) { os->emplace(decl, *result_ptr); }
  }
}

OverloadSet::OverloadSet(base::PtrSpan<Declaration const> decls,
                         compiler::Compiler *visitor) {
  EmplaceDecls(this, decls, visitor);
}

// TODO only hold functions?
OverloadSet::OverloadSet(core::Scope *scope, std::string_view id,
                         compiler::Compiler *visitor) {
  EmplaceDecls(this, scope->AllDeclsWithId(id), visitor);
}

void OverloadSet::add_adl(std::string_view id, type::Type const *t) {
  absl::flat_hash_set<module::Module const *> modules;
  t->defining_modules(&modules);

  for (auto *mod : modules) {
    ASSIGN_OR(continue, auto &d, mod->GetDecl(id));
    // TODO remove this const_cast.
    ASSIGN_OR(continue, auto &t,
              compiler::Compiler(const_cast<module::Module *>(mod)).type_of(&d));
    // TODO handle this case. I think it's safe to just discard it.

    for (auto const &overload : *this) {
      if (&d == overload.expr) { return; }
    }
    // TODO const
    emplace(&d, compiler::VerifyResult{&t, true});
  }
}

}  // namespace ast
