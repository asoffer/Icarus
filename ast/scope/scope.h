#ifndef ICARUS_AST_SCOPE_SCOPE_H
#define ICARUS_AST_SCOPE_SCOPE_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "ast/ast_fwd.h"
#include "base/cast.h"

namespace module {
struct BasicModule;
}  // namespace module

namespace ast {

struct Scope : public base::Cast<Scope> {
  Scope() = delete;
  Scope(Scope *parent) : parent(parent) {}
  virtual ~Scope() {}

  template <typename ScopeType, typename... Args>
  std::unique_ptr<ScopeType> add_child(Args &&... args) {
    return std::make_unique<ScopeType>(this, std::forward<Args>(args)...);
  }

  std::vector<Declaration const *> AllDeclsWithId(std::string_view id) const;

  void InsertDecl(std::string_view id, Declaration *decl);

  template <typename Sc>
  Sc const *Containing() const {
    Scope const *scope = this;
    while (scope and not scope->is<Sc>()) { scope = scope->parent; }
    return static_cast<Sc const *>(scope);
  }

  template <typename Sc>
  Sc *Containing() {
    Scope *scope = this;
    while (scope and not scope->is<Sc>()) { scope = scope->parent; }
    return static_cast<Sc *>(scope);
  }

  absl::flat_hash_map<std::string_view, std::vector<Declaration *>> decls_;
  absl::flat_hash_map<std::string_view, std::vector<Declaration *>> child_decls_;

  absl::flat_hash_set<module::BasicModule const *> embedded_modules_;
  Scope *parent = nullptr;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_SCOPE_H
