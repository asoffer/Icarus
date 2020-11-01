#ifndef ICARUS_AST_SCOPE_SCOPE_H
#define ICARUS_AST_SCOPE_SCOPE_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/types/span.h"
#include "ast/ast_fwd.h"
#include "base/cast.h"
#include "base/debug.h"
#include "base/log.h"

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

  // TODO: The `id` needs to be the same as decl->id() but due to layering
  // issues declarations are incomplete when scopes are being compiled.
  void InsertDecl(std::string_view id, Declaration *decl);

  // Whether or not non-constant declarations are visible across this scope
  // boundary. In this example,
  //
  // ```
  // f ::= (n: int64) -> () {
  //   m := 1
  //   while (m != 10) do {
  //     m := "hello"
  //   }
  // }
  //
  // n := 1
  // ```
  //
  // The identifier `m` is visible across the while-scope boundary, so `m` would
  // be an ambiguous redeclaration. However the identifier `n` is not visible
  // because function scopes constitute a visibility boundary.
  virtual bool is_visibility_boundary() const { return false; }

  template <typename Sc>
  Sc const *Containing() const {
    Scope const *scope = this;
    LOG("scope", "Looking for ancestor of type %s", typeid(Sc).name());
    while (scope and not scope->is<Sc>()) {
      LOG("scope", "%p => %p", scope, scope->parent);
      scope = scope->parent;
    }
    return static_cast<Sc const *>(scope);
  }

  template <typename Sc>
  Sc *Containing() {
    return const_cast<Sc *>(
        static_cast<Scope const *>(this)->template Containing<Sc>());
  }

  absl::flat_hash_map<std::string_view, std::vector<Declaration *>> decls_;

  absl::Span<Declaration *const> VisibleChildren(std::string_view id) const {
    if (auto iter = child_decls_.find(id); iter != child_decls_.end()) {
      return iter->second;
    }
    return absl::Span<Declaration *const>();
  }

 private:
  absl::flat_hash_map<std::string_view, std::vector<Declaration *>>
      child_decls_;

 public:
  absl::flat_hash_set<module::BasicModule const *> embedded_modules_;
  Scope *parent = nullptr;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_SCOPE_H
