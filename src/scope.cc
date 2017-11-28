#include "scope.h"

#include "ast/ast.h"
#include "type/type.h"

DeclScope *Scope::Global = new DeclScope(nullptr);

AST::Declaration *Scope::DeclHereOrNull(const std::string &name,
                                        Type *declared_type) {
  for (auto decl : decls_) {
    if (decl->type == declared_type && decl->identifier->token == name) {
      return decl;
    }
  }
  return nullptr;
}

AST::Declaration *Scope::DeclReferencedOrNull(const std::string &name,
                                              Type *declared_type) {
  for (auto scope_ptr = this; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto ptr = scope_ptr->DeclHereOrNull(name, declared_type);
    if (ptr) { return ptr; }
  }
  return nullptr;
}

AST::Identifier *Scope::IdHereOrNull(const std::string &name) {
  for (auto decl : decls_) {
    if (decl->identifier->token == name) { return decl->identifier.get(); }
  }
  return nullptr;
}

AST::Identifier *Scope::IdReferencedOrNull(const std::string &name) {
  for (auto scope_ptr = this; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto ptr = scope_ptr->IdHereOrNull(name);
    if (ptr) { return ptr; }
  }
  return nullptr;
}

Type *Scope::FunctionTypeReferencedOrNull(const std::string &fn_name,
                                          Type *input_type) {
  for (auto scope_ptr = this; scope_ptr; scope_ptr = scope_ptr->parent) {
    auto id_ptr = scope_ptr->IdHereOrNull(fn_name);
    if (!id_ptr) { continue; }

    if (!id_ptr->type) {
      // NOTE: IdHereOrNull always returns the identifier bound to the
      // declaration, so if the type isn't specified, we need to actually verify
      // the type of it's declaration.
      ASSERT(id_ptr->decl, "");
      id_ptr->decl->verify_types();
      ASSERT(id_ptr->type, "");
    }

    ASSERT(id_ptr->type->is<Function>(), "");
    auto fn_type = (Function *)id_ptr->type;
    if (fn_type->input == input_type) { return fn_type; }
  }
  return nullptr;
}

IR::Val Scope::FuncHereOrNull(const std::string &fn_name, Function *fn_type) {
  Scope *scope_ptr = this;
  AST::Declaration *decl;

  decl = DeclReferencedOrNull(fn_name, fn_type);
  for (; scope_ptr; scope_ptr = scope_ptr->parent) {
    decl = scope_ptr->DeclHereOrNull(fn_name, fn_type);
    if (decl) { break; }
  }

  if (!decl) { return IR::Val::None(); }

  if (decl->addr == IR::Val::None()) {
    if (decl->init_val->is<AST::FunctionLiteral>()) {
      auto old_func  = IR::Func::Current;
      auto old_block = IR::Block::Current;

      decl->addr = decl->init_val->EmitIR(IR::Cmd::Kind::Exec);
      decl->addr.value.as<IR::Func *>()->name =
          Mangle(fn_type, decl->identifier.get(), scope_ptr);

      IR::Func::Current  = old_func;
      IR::Block::Current = old_block;
    } else {
      NOT_YET();
    }
  }

  ASSERT(decl->addr != IR::Val::None(), "");
  return IR::Load(decl->addr);
}

std::vector<AST::Declaration *> Scope::AllDeclsWithId(const std::string &id) {
  std::vector<AST::Declaration *> matching_decls;
  for (auto scope_ptr = this; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    for (auto decl : scope_ptr->decls_) {
      if (decl->identifier->token != id) { continue; }
      decl->verify_types();
      if (decl->type == Err) { continue; }
      matching_decls.push_back(decl);
    }
  }
  return matching_decls;
}

ExecScope::ExecScope(Scope *parent) : Scope(parent) {
  // If this scope is a FnScope it will be handled by the FnScope constructor.
  auto containing_fn_scope = parent->ContainingFnScope();
  if (containing_fn_scope) { containing_fn_scope->innards_.push_back(this); }
}

void ExecScope::Enter() const {
  for (auto *decl : decls_) {
    if (decl->is<AST::InDecl>()) { continue; }
    decl->EmitIR(IR::Cmd::Kind::Exec);
  }
}
void ExecScope::Exit() const {
  for (auto *decl : decls_) { decl->type->EmitDestroy(decl->addr); }
}
