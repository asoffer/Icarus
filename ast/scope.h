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

struct Scope : base::Cast<Scope> {
  enum class Kind {
    Declarative,
    Executable,
  };

  Scope() = delete;
  Scope(Scope *parent, bool executable);

  Scope(module::BasicModule *module, bool executable);
  Scope(Kind kind, Scope *parent, bool executable);
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

  Scope *parent() {
    return parent_ & 1 ? nullptr : reinterpret_cast<Scope *>(parent_);
  }
  Scope const *parent() const {
    return parent_ & 1 ? nullptr : reinterpret_cast<Scope *>(parent_);
  }

  module::BasicModule &module() {
    return parent_ & 1
               ? *ASSERT_NOT_NULL(
                     reinterpret_cast<module::BasicModule *>(parent_ - 1))
               : parent()->module();
  }

  module::BasicModule const &module() const {
    return parent_ & 1
               ? *ASSERT_NOT_NULL(
                     reinterpret_cast<module::BasicModule *>(parent_ - 1))
               : parent()->module();
  }

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
    while (scope and not scope->is<Sc>()) { scope = scope->parent(); }
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

  void embed(module::BasicModule const *module) {
    embedded_modules_.insert(module);
  }

  absl::flat_hash_set<module::BasicModule const *> const &embedded_modules()
      const {
    return embedded_modules_;
  }

 private:
  uintptr_t parent_ = 0;
  absl::flat_hash_map<std::string_view, std::vector<Declaration::Id const *>>
      child_decls_;
  absl::flat_hash_set<module::BasicModule const *> embedded_modules_;
  bool executable_;
};

// An executable scope representing the body of a function literal. These scopes
// need to know about all child scopes so they can stack-allocate enough space
// when they start.
//
// TODO: Rename this as it represents not just functions but any executable
// scope at the root of execution.
struct FnScope : Scope {
  explicit FnScope(module::BasicModule *module) : Scope(module, true) {
    descendants_.push_back(this);
  }

  explicit FnScope(Scope *parent) : Scope(parent, true) {
    descendants_.push_back(this);
  }

  bool is_visibility_boundary() const override { return true; }

  void insert_descendant(Scope *s) { descendants_.push_back(s); }
  absl::Span<Scope *const> descendants() const { return descendants_; }

 private:
  std::vector<Scope *> descendants_;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_H
