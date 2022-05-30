#include "ast/scope.h"

namespace ast {

void Scope::InsertDeclaration(ast::Declaration const *decl) {
  if (not(decl->flags() & Declaration::f_IsConst)) {
    ordered_non_constant_declarations_.push_back(decl);
  }
  for (auto const &id : decl->ids()) {
    if (id.name() == "move" or id.name() == "copy" or id.name() == "destroy") {
      continue;
    }
    LOG("Scope", "Inserting a declaration of `%s` into %p", id.name(), this);
    decls_[id.name()].push_back(&id);
    for (auto *scope_ptr = parent(); scope_ptr;
         scope_ptr       = scope_ptr->parent()) {
      if (kind_ == Kind::BoundaryExecutable) { break; }
      child_decls_[id.name()].push_back(&id);
    }
  }
}

Scope::Scope(Kind kind) : kind_(kind) {
  executable_descendants_.push_back(this);
}

Scope::Scope(module::Module *module)
    : parent_(reinterpret_cast<uintptr_t>(module) | 1),
      kind_(Kind::BoundaryExecutable) {
  executable_descendants_.push_back(this);
}

void Scope::set_parent(Scope *p) {
  ASSERT(parent_ == 0u);
  parent_  = reinterpret_cast<uintptr_t>(p);
  Scope *s = this;
  if (s->kind() != Kind::BoundaryExecutable) {
    p->ordered_non_constant_declarations_.push_back(this);
  }
  while (s->kind() != Kind::BoundaryExecutable) { s = s->parent(); }
  if (s != this) { s->executable_descendants_.push_back(this); }
}

Scope::visible_declaration_ancestor_iterator::
    visible_declaration_ancestor_iterator(Scope const *p, std::string_view name)
    : p_(ASSERT_NOT_NULL(p)) {
  GetDeclsAndFindNext(name);
}

void Scope::visible_declaration_ancestor_iterator::FindNext(
    std::string_view name) {
  if (only_constants_) {
    auto iter = std::find_if(
        ids_.begin(), ids_.end(), [](ast::Declaration::Id const *id) {
          return id->declaration().flags() & ast::Declaration::f_IsConst;
        });
    ids_.remove_prefix(std::distance(ids_.begin(), iter));
  }

  if (ids_.empty()) { IncrementScope(name); }
}

void Scope::visible_declaration_ancestor_iterator::GetDeclsAndFindNext(
    std::string_view name) {
  if (auto iter = p_->decls_.find(name); iter == p_->decls_.end()) {
    IncrementScope(name);
  } else {
    ids_ = iter->second;
    ASSERT(ids_.size() != 0);
    FindNext(name);
  }
}

void Scope::visible_declaration_ancestor_iterator::IncrementScope(
    std::string_view name) {
  switch (ASSERT_NOT_NULL(p_)->kind()) {
    case Kind::Declarative:
    case Kind::BoundaryExecutable: only_constants_ = true; break;
    case Kind::Executable: break;
  }
  p_ = p_->parent();

  if (not p_) {
    ids_ = absl::Span<Declaration::Id const *const>(nullptr, 0);
  } else {
    GetDeclsAndFindNext(name);
  }
}

}  // namespace ast
