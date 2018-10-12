#include "scope.h"

#include "ast/declaration.h"
#include "ast/identifier.h"
#include "context.h"
#include "ir/cmd.h"
#include "module.h"
#include "type/function.h"
#include "type/pointer.h"

void Scope::InsertDecl(AST::Declaration *decl) {
  decls_[decl->identifier->token].push_back(decl);
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    scope_ptr->child_decls_[decl->identifier->token].push_back(decl);
  }
}

// TODO error version will always have nullptr types.
std::pair<base::vector<TypedDecl>, base::vector<TypedDecl>>
Scope::AllDeclsWithId(std::string const &id, Context *ctx) {
  base::vector<TypedDecl> matching_decls, matching_error_decls;
  for (auto scope_ptr = this; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    auto iter = scope_ptr->decls_.find(id);
    if (iter == scope_ptr->decls_.end()) { continue; }
    for (const auto &decl : iter->second) {
      auto *t = decl->VerifyType(ctx);
      (t == nullptr ? matching_error_decls : matching_decls)
          .emplace_back(t, decl);
    }
  }

  for (auto const *mod : ctx->mod_->embedded_modules_) {
    if (auto *decl = mod->GetDecl(id)) {
      matching_decls.emplace_back(mod->type_of(AST::BoundConstants{}, decl),
                                  decl);
    }
  }
  return std::pair(std::move(matching_decls), std::move(matching_error_decls));
}

ExecScope::ExecScope(Scope *parent) : Scope(parent) {
  // If this scope is a FnScope it will be handled by the FnScope constructor.
  auto containing_fn_scope = parent->ContainingFnScope();
  if (containing_fn_scope) { containing_fn_scope->innards_.push_back(this); }
}

// TODO not sure you need to pass in the module.
void FnScope::MakeAllStackAllocations(Context *ctx) {
  for (auto *scope : innards_) {
    for (const auto & [ key, val ] : scope->decls_) {
      for (auto *decl : val) {
        if (decl->const_ || decl->is_arg_) { continue; }

        // TODO it's wrong to use a default BoundConstants, but it's even more
        // wrong to store the address on the declaration, so you can fix those
        // together.
        ctx->set_addr(decl, IR::Alloca(ctx->type_of(decl)));
      }
    }
  }
}
