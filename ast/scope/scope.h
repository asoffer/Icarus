#ifndef ICARUS_AST_SCOPE_SCOPE_H
#define ICARUS_AST_SCOPE_SCOPE_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "absl/container/flat_hash_set.h"
#include "absl/types/span.h"
#include "ast/ast_fwd.h"
#include "ast/declaration.h"
#include "base/cast.h"
#include "base/debug.h"
#include "base/iterator.h"
#include "base/log.h"

namespace ast {
struct ModuleScope;

struct Scope : public base::Cast<Scope> {
  Scope() = delete;
  Scope(Scope *parent, bool executable)
      : parent_(parent), executable_(executable) {}
  virtual ~Scope() {}

  struct ancestor_iterator {
    friend bool operator==(ancestor_iterator lhs, ancestor_iterator rhs) {
      return lhs.p_ == rhs.p_;
    }

    friend bool operator!=(ancestor_iterator lhs, ancestor_iterator rhs) {
      return not(lhs == rhs);
    }

    Scope const &operator*() const { return *p_; }

    ancestor_iterator operator++() {
      p_ = p_->parent();
      return *this;
    }

    ancestor_iterator operator++(int) {
      auto copy = *this;
      ++*this;
      return copy;
    }

   private:
    friend struct Scope;

    constexpr ancestor_iterator(Scope const *p) : p_(p) {}

    Scope const *p_;
  };

  Scope *parent() { return parent_; }
  Scope const *parent() const { return parent_; }

  ancestor_iterator begin() const { return ancestor_iterator(this); }
  ancestor_iterator end() const { return ancestor_iterator(nullptr); }
  auto ancestors() const { return base::iterator_range(begin(), end()); }

  void InsertDeclaration(ast::Declaration const * decl);

  // Whether or not non-constant declarations are visible across this scope
  // boundary. In this example,
  //
  // ```
  // f ::= (n: i64) -> () {
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
  bool executable() const { return executable_; }

  template <typename Sc>
  Sc const *Containing() const {
    Scope const *scope = this;
    LOG("scope", "Looking for ancestor of type %s", typeid(Sc).name());
    while (scope and not scope->is<Sc>()) {
      LOG("scope", "%p => %p", scope, scope->parent());
      scope = scope->parent();
    }
    return static_cast<Sc const *>(scope);
  }

  template <typename Sc>
  Sc *Containing() {
    return const_cast<Sc *>(
        static_cast<Scope const *>(this)->template Containing<Sc>());
  }

  absl::flat_hash_map<std::string_view, std::vector<Declaration::Id const *>>
      decls_;

  absl::Span<Declaration::Id const *const> VisibleChildren(
      std::string_view name) const {
    if (auto iter = child_decls_.find(name); iter != child_decls_.end()) {
      return iter->second;
    }
    return absl::Span<Declaration::Id const *const>();
  }

  void embed(ast::ModuleScope const *scope) {
    embedded_module_scopes_.insert(scope);
  }

  absl::flat_hash_set<ModuleScope const *> const &embedded_module_scopes()
      const {
    return embedded_module_scopes_;
  }

 private:
  Scope *parent_ = nullptr;
  absl::flat_hash_map<std::string_view, std::vector<Declaration::Id const *>>
      child_decls_;
  absl::flat_hash_set<ModuleScope const *> embedded_module_scopes_;
  bool executable_;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_SCOPE_H
