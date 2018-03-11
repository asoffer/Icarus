#include "scope.h"

#include "ast/ast.h"
#include "context.h"
#include "ir/func.h"
#include "type/type.h"

DeclScope *GlobalScope = new DeclScope(nullptr);

AST::Declaration *Scope::DeclHereOrNull(const std::string &name,
                                        Type *declared_type) {
  auto iter = decls_.find(name);
  if (iter == decls_.end()) { return nullptr; }
  for (auto decl : iter->second) {
    if (decl->type == declared_type) { return decl; }
  }
  return nullptr;
}

AST::Declaration *Scope::DeclReferencedOrNull(const std::string &name,
                                              Type *declared_type) {
  for (auto scope_ptr = this; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto ptr = scope_ptr->DeclHereOrNull(name, declared_type);
    if (ptr != nullptr) { return ptr; }
  }
  return nullptr;
}

// TODO this is dangerous
AST::Identifier *Scope::IdHereOrNull(const std::string &name) const {
  auto iter = decls_.find(name);
  if (iter == decls_.end()) { return nullptr; }
  if (iter->second.empty()) { return nullptr; }
  return iter->second[0]->identifier.get();
}

AST::Identifier *Scope::IdReferencedOrNull(const std::string &name) {
  for (auto scope_ptr = this; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto ptr = scope_ptr->IdHereOrNull(name);
    if (ptr) { return ptr; }
  }
  return nullptr;
}

const Type *
Scope::FunctionTypeReferencedOrNull(const std::string &fn_name,
                                    std::vector<const Type *> input_type) {
  for (auto scope_ptr = this; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto id_ptr = scope_ptr->IdHereOrNull(fn_name);
    if (!id_ptr) { continue; }

    if (!id_ptr->type) {
      // NOTE: IdHereOrNull always returns the identifier bound to the
      // declaration, so if the type isn't specified, we need to actually verify
      // the type of it's declaration.
      ASSERT(id_ptr->decl, "");
      // TODO bound constants?
      Context ctx;
      id_ptr->decl->Validate(&ctx);
      ASSERT(id_ptr->type, "");
    }

    ASSERT_TYPE(Function, id_ptr->type);
    if (id_ptr->type->as<Function>().input == input_type) {
      return id_ptr->type;
    }
  }
  return nullptr;
}

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
      decl->Validate(ctx);
      (decl->type == Err ? matching_error_decls : matching_decls)
          .push_back(decl);
    }
  }
  return std::pair(std::move(matching_decls), std::move(matching_error_decls));
}

ExecScope::ExecScope(Scope *parent) : Scope(parent) {
  // If this scope is a FnScope it will be handled by the FnScope constructor.
  auto containing_fn_scope = parent->ContainingFnScope();
  if (containing_fn_scope) { containing_fn_scope->innards_.push_back(this); }
}

void ExecScope::Enter() const {
  ForEachDeclHere(+[](AST::Declaration *decl) {
    if (decl->const_) { return; }
    if (!decl->is<AST::InDecl>()) { 
    Context ctx;
    decl->EmitIR(&ctx);
    }
  });
}

void ExecScope::Exit() const {
  ForEachDeclHere(+[](AST::Declaration *decl) {
    if (decl->const_) { return; }
    decl->type->EmitDestroy(decl->addr);
  });
}
