#ifndef ICARUS_AST_SCOPE_H
#define ICARUS_AST_SCOPE_H

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

namespace module {
struct BasicModule;
}  // namespace module

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

  void InsertDeclaration(Declaration const *decl);

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

  void embed(ModuleScope const *scope) {
    embedded_module_scopes_.insert(scope);
  }

  absl::flat_hash_set<ModuleScope const *> const &embedded_module_scopes()
      const {
    return embedded_module_scopes_;
  }

  // Calls `fn` on each declaration in this scope and in parent scopes with the
  // given identifier `name`, until calling `fn` returns false.
  bool ForEachDeclIdTowardsRoot(
      std::string_view name,
      std::invocable<Declaration::Id const *> auto &&fn) const;

 private:
  Scope *parent_ = nullptr;
  absl::flat_hash_map<std::string_view, std::vector<Declaration::Id const *>>
      child_decls_;
  absl::flat_hash_set<ModuleScope const *> embedded_module_scopes_;
  bool executable_;
};

// An executable scope representing the body of a function literal. These scopes
// need to know about all child scopes so they can stack-allocate enough space
// when they start.
struct FnScope : Scope {
  FnScope(Scope *parent) : Scope(parent, true) { descendants_.push_back(this); }

  bool is_visibility_boundary() const override { return true; }

  void insert_descendant(Scope *s) { descendants_.push_back(s); }
  absl::Span<Scope *const> descendants() const { return descendants_; }

 private:
  std::vector<Scope *> descendants_;
};

// TODO using FnScope for modules makes sense for executable modules and isn't
// super harmful for library modules, but also isn't entirely correct. Figure
// out the semantically correct solution here, probably extracting some of the
// functionality of FnScope.
struct ModuleScope : FnScope {
  ModuleScope(module::BasicModule *mod) : FnScope(nullptr), module_(mod) {}

  module::BasicModule *module() { return module_; }
  module::BasicModule const *module() const { return module_; }

  absl::Span<Declaration::Id const *const> ExportedDeclarationIds(
      std::string_view name) const {
    auto iter = exported_declarations_.find(name);
    if (iter == exported_declarations_.end()) { return {}; }

    // TODO: handle exported embedded modules here too.
    return iter->second;
  }

  void insert_exported(Declaration::Id const *id) {
    exported_declarations_[id->name()].push_back(id);
  }

 private:
  friend struct Scope;

  absl::flat_hash_map<std::string_view, std::vector<Declaration::Id const *>>
      exported_declarations_;

  module::BasicModule *module_;
};

// Represents a scope whose contents can only be declarations. This is in
// contrast with scopes that can have other types of statements. For example, a
// struct is a declarative scope because all entries inside that scope must be
// declarations.
struct DeclScope : public Scope {
  DeclScope(Scope *parent) : Scope(parent, false) {}
  ~DeclScope() override {}
};

// Represents the body of a scope-literal which is the construct responsible for
// defining scopes.
struct ScopeLitScope : public DeclScope {
  ScopeLitScope(Scope *parent) : DeclScope(parent) {}
};

bool Scope::ForEachDeclIdTowardsRoot(
    std::string_view name,
    std::invocable<Declaration::Id const *> auto &&fn) const {
  for (Scope const &s : ancestors()) {
    if (auto iter = s.decls_.find(name); iter != s.decls_.end()) {
      for (auto const *id : iter->second) {
        if (not fn(id)) { return false; }
      }
    }

    for (auto const *mod_scope : s.embedded_module_scopes()) {
      for (auto const *id : mod_scope->ExportedDeclarationIds(name)) {
        // TODO what about transitivity for embedded modules?
        // New context will lookup with no constants.
        if (not fn(id)) { return false; }
      }
    }
  }
  return true;
}

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_H
