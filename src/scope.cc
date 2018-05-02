#include "scope.h"

#include "ast/declaration.h"
#include "ast/identifier.h"
#include "context.h"
#include "ir/func.h"
#include "module.h"
#include "type/function.h"

void Scope::InsertDecl(AST::Declaration *decl) {
  decls_[decl->identifier->token].push_back(decl);
  if (parent == nullptr) { return; }
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    scope_ptr->child_decls_[decl->identifier->token].push_back(decl);
  }
}

std::pair<std::vector<AST::Declaration *>, std::vector<AST::Declaration *>>
Scope::AllDeclsWithId(const std::string &id, Context *ctx) {
  std::vector<AST::Declaration *> matching_decls, matching_error_decls;
  for (auto scope_ptr = this; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    auto iter = scope_ptr->decls_.find(id);
    if (iter == scope_ptr->decls_.end()) { continue; }
    for (const auto &decl : iter->second) {
      decl->VerifyType(ctx);
      (decl->type == type::Err ? matching_error_decls : matching_decls)
          .push_back(decl);
    }
  }

  for (const auto* mod : ctx->mod_->embedded_modules_) {
    if (auto *decl = mod->GetDecl(id)) { matching_decls.push_back(decl); }
  }
  return std::pair(std::move(matching_decls), std::move(matching_error_decls));
}

ExecScope::ExecScope(Scope *parent) : Scope(parent) {
  // If this scope is a FnScope it will be handled by the FnScope constructor.
  auto containing_fn_scope = parent->ContainingFnScope();
  if (containing_fn_scope) { containing_fn_scope->innards_.push_back(this); }
}

void ExecScope::Enter(Context* ctx) const {
  ForEachDeclHere([ctx](AST::Declaration *decl) {
    if (!decl->const_) { decl->EmitIR(ctx); }
  });
}

void ExecScope::Exit(Context *ctx) const {
  ForEachDeclHere([ctx](AST::Declaration *decl) {
    if (decl->const_) { return; }
    decl->type->EmitDestroy(decl->addr, ctx);
  });
}
