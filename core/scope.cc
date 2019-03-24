#include "core/scope.h"

#include "ast/declaration.h"
#include "ast/identifier.h"
#include "ast/match_declaration.h"
#include "misc/context.h"
#include "ir/cmd.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/pointer.h"

namespace core {

void Scope::InsertDecl(ast::Declaration *decl) {
  decls_[decl->id_].push_back(decl);
  for (auto *scope_ptr = parent; scope_ptr; scope_ptr = scope_ptr->parent) {
    scope_ptr->child_decls_[decl->id_].push_back(decl);
  }
}

Module const *Scope::module() const {
  if (auto *ds = this->if_as<ModuleScope>()) { return ds->module_; }
  return parent->module();
}

// TODO error version will always have nullptr types.
std::vector<type::Typed<ast::Declaration *>> Scope::AllDeclsWithId(
    std::string const &id, Context *ctx) const {
  std::vector<type::Typed<ast::Declaration *>> matching_decls;
  for (auto scope_ptr = this; scope_ptr != nullptr;
       scope_ptr      = scope_ptr->parent) {
    if (auto iter = scope_ptr->decls_.find(id);
        iter != scope_ptr->decls_.end()) {
      for (auto *decl : iter->second) {
        auto *t = ctx->type_of(decl);
        // TODO This will call VerifyType once if it's correct, but *every* time
        // if it's incorrect. Fix this.
        if (t == nullptr) { t = decl->VerifyType(ctx).type_; }
        matching_decls.emplace_back(decl, t);
      }
    }

    for (auto const *mod : scope_ptr->embedded_modules_) {
      // TODO use the right bound constants? or kill bound constants?
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
    for (const auto &[key, val] : scope->decls_) {
      for (auto *decl : val) {
        if (decl->const_ || decl->is_fn_param_ ||
            decl->is<ast::MatchDeclaration>()) {
          continue;
        }

        // TODO it's wrong to use a default BoundConstants, but it's even more
        // wrong to store the address on the declaration, so you can fix those
        // together.
        ctx->set_addr(decl, ir::Alloca(ctx->type_of(decl)));
      }
    }
  }
}

void ExecScope::MakeAllDestructions(Context *ctx) {
  // TODO store these in the appropriate order so we don't have to compute this?
  // Will this be faster?
  std::vector<ast::Declaration *> ordered_decls;
  for (auto &[name, decls] : decls_) {
    ordered_decls.insert(ordered_decls.end(), decls.begin(), decls.end());
  }

  // TODO eek, don't use line number to determine destruction order!
  std::sort(ordered_decls.begin(), ordered_decls.end(),
            [](ast::Declaration *lhs, ast::Declaration *rhs) {
              return (lhs->span.start.line_num > rhs->span.start.line_num) ||
                     (lhs->span.start.line_num == rhs->span.start.line_num &&
                      lhs->span.start.offset > rhs->span.start.offset);
            });

  for (auto *decl : ordered_decls) {
    auto *t = ASSERT_NOT_NULL(ctx->type_of(decl));
    if (!t->needs_destroy()) { continue; }
    t->EmitDestroy(ctx->addr(decl), ctx);
  }
}

}  // namespace core
