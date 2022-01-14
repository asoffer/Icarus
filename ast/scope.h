#ifndef ICARUS_AST_SCOPE_H
#define ICARUS_AST_SCOPE_H

#include <concepts>
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
#include "base/ptr_union.h"

namespace module {
struct BasicModule;
}  // namespace module

namespace ast {

struct Scope : base::Cast<Scope> {
  enum class Kind {
    Declarative,
    BoundaryExecutable,
    Executable,
  };

  explicit Scope(Kind kind);
  explicit Scope(module::BasicModule *module);

  Kind kind() const { return kind_; }

  struct ancestor_iterator {
    bool operator==(ancestor_iterator const &lhs) const = default;
    bool operator!=(ancestor_iterator const &lhs) const = default;

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

  void set_parent(Scope *p);

  Scope *parent() {
    ASSERT(parent_ != 0u);
    return parent_ & 1 ? nullptr : reinterpret_cast<Scope *>(parent_);
  }
  Scope const *parent() const {
    ASSERT(parent_ != 0u);
    return parent_ & 1 ? nullptr : reinterpret_cast<Scope *>(parent_);
  }

  module::BasicModule &module() {
    ASSERT(parent_ != 0u);
    return parent_ & 1
               ? *ASSERT_NOT_NULL(
                     reinterpret_cast<module::BasicModule *>(parent_ - 1))
               : parent()->module();
  }

  module::BasicModule const &module() const {
    ASSERT(parent_ != 0u);
    return parent_ & 1
               ? *ASSERT_NOT_NULL(
                     reinterpret_cast<module::BasicModule *>(parent_ - 1))
               : parent()->module();
  }

  auto ancestors() const {
    return base::iterator_range(ancestor_iterator(this),
                                ancestor_iterator(nullptr));
  }

  void InsertDeclaration(Declaration const *decl);

  absl::flat_hash_map<std::string_view, std::vector<Declaration::Id const *>>
      decls_;

  // Invokes `f` on each declaration in this and all descendant scopes in the
  // order of occurence.
  void ForEachDeclaration(std::invocable<Declaration const *> auto &&f) const {
    for (auto p : ordered_declarations_) {
      if (auto const *decl = p.get_if<Declaration>()) {
        f(decl);
      } else {
        p.get<Scope>()->ForEachDeclaration(f);
      }
    }
  }

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

  // Returns all scopes that are part of the executable scope. Requires that
  // this scope be a root of execution (i.e., have Kind BoundaryExecutable).
  absl::Span<Scope *const> executable_descendants() const {
    ASSERT(kind_ == Kind::BoundaryExecutable);
    return executable_descendants_;
  }

 private:
  uintptr_t parent_ = 0;
  Kind kind_;
  // TODO: These are only useful in the shadowing check, so probably not worth
  // storing.
  absl::flat_hash_map<std::string_view, std::vector<Declaration::Id const *>>
      child_decls_;
  absl::flat_hash_set<module::BasicModule const *> embedded_modules_;
  std::vector<Scope *> executable_descendants_;

  // Sequence consisting of pointers to either a declaration or a child scope in
  // the order that they appear.
  std::vector<base::PtrUnion<Declaration const, Scope const>>
      ordered_declarations_;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_H
