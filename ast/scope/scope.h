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

  void InsertDecl(std::string_view id, Declaration *decl);

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

  absl::Span<Declaration *const> children_with_id(std::string_view id) const {
    auto iter = child_decls_.find(id);
    if (iter == child_decls_.end()) { return absl::Span<Declaration *const>(); }
    return iter->second;
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
