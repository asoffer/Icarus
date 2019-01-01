#include "scope.h"

#include "ast/declaration.h"
#include "ast/identifier.h"
#include "context.h"
#include "ir/cmd.h"
#include "module.h"
#include "type/function.h"
#include "type/pointer.h"

void Scope::InsertDecl(ast::Declaration *decl) {
  decls_[decl->id_].push_back(decl);
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    scope_ptr->child_decls_[decl->id_].push_back(decl);
  }
}

// TODO error version will always have nullptr types.
base::vector<type::Typed<ast::Declaration *>> Scope::AllDeclsWithId(
    std::string const &id, Context *ctx) {
  base::vector<type::Typed<ast::Declaration *>> matching_decls;
  for (auto scope_ptr = this; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    auto iter = scope_ptr->decls_.find(id);
    if (iter != scope_ptr->decls_.end()) {
      for (auto *decl : iter->second) {
        auto *t = ctx->type_of(decl);
        if (t == nullptr) { t = decl->VerifyType(ctx); }
        matching_decls.emplace_back(decl, t);
      }
    }

    for (auto const *mod : scope_ptr->embedded_modules_) {
      if (auto *decl = mod->GetDecl(id)) {
        matching_decls.emplace_back(decl,
                                    mod->type_of(ast::BoundConstants{}, decl));
      }
    }
  }
  return matching_decls;
}

ExecScope::ExecScope(Scope *parent) : Scope(parent) {
  // If this scope is a FnScope it will be handled by the FnScope constructor.
  if (auto containing_fn_scope = parent->ContainingFnScope()) {
    containing_fn_scope->innards_.push_back(this);
  }
}

// TODO not sure you need to pass in the module.
void FnScope::MakeAllStackAllocations(Context *ctx) {
  for (auto *scope : innards_) {
    for (const auto & [ key, val ] : scope->decls_) {
      for (auto *decl : val) {
        if (decl->const_ || decl->is_fn_param_) { continue; }

        // TODO it's wrong to use a default BoundConstants, but it's even more
        // wrong to store the address on the declaration, so you can fix those
        // together.
        ctx->set_addr(decl, ir::Alloca(ctx->type_of(decl)));
      }
    }
  }
}

void Scope::MakeAllDestructions(Context *ctx) {
  // TODO order these correctly.
  for (auto & [ name, decls ] : decls_) {
    for (auto *decl : decls) {
      auto *t = ASSERT_NOT_NULL(ctx->type_of(decl));
      if (!t->needs_destroy()) { continue; }
      t->EmitDestroy(ctx->addr(decl), ctx);
    }
  }
}
