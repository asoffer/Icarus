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
struct Module;
}  // namespace module

namespace ast {

struct Scope : base::Cast<Scope> {
  using code_location_t = base::PtrUnion<Declaration const, Scope const>;

  enum class Kind {
    Declarative,
    BoundaryExecutable,
    Executable,
  };

  explicit Scope(Kind kind);
  explicit Scope(module::Module *module);

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

  struct visible_declaration_ancestor_iterator {
    friend bool operator==(visible_declaration_ancestor_iterator const &lhs,
                           visible_declaration_ancestor_iterator const &rhs) {
      return lhs.p_ == rhs.p_ and lhs.ids_.data() == rhs.ids_.data();
    }
    friend bool operator!=(visible_declaration_ancestor_iterator const &lhs,
                           visible_declaration_ancestor_iterator const &rhs) {
      return not(lhs == rhs);
    }

    ast::Declaration::Id const &operator*() const { return *ids_[0]; }

    visible_declaration_ancestor_iterator operator++() {
      std::string_view name = ids_[0]->name();
      ids_.remove_prefix(1);
      FindNext(name);
      return *this;
    }

    visible_declaration_ancestor_iterator operator++(int) {
      auto copy = *this;
      ++*this;
      return copy;
    }

   private:
    friend struct Scope;

    constexpr visible_declaration_ancestor_iterator() : p_(nullptr) {}

    visible_declaration_ancestor_iterator(Scope const *p,
                                          std::string_view name);


    void FindNext(std::string_view name);
    void GetDeclsAndFindNext(std::string_view name);
    void IncrementScope(std::string_view name);

    // TODO: We can improve efficiency by stashing `only_constants_` in the low
    // bit of `p_`.
    Scope const *p_;
    absl::Span<Declaration::Id const *const> ids_;
    bool only_constants_ = false;
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

  module::Module &module() {
    ASSERT(parent_ != 0u);
    return parent_ & 1 ? *ASSERT_NOT_NULL(
                             reinterpret_cast<module::Module *>(parent_ - 1))
                       : parent()->module();
  }

  module::Module const &module() const {
    ASSERT(parent_ != 0u);
    return parent_ & 1 ? *ASSERT_NOT_NULL(
                             reinterpret_cast<module::Module *>(parent_ - 1))
                       : parent()->module();
  }

  auto ancestors() const {
    return base::iterator_range(ancestor_iterator(this),
                                ancestor_iterator(nullptr));
  }

  // Returns an iterator range which iterates through all
  // accessible `ast::Declaration::Id`s in this or ancestor scopes which are
  // visible and have name `name`.
  auto ancestor_declaration_id_named(std::string_view name) const {
    return base::iterator_range(
        visible_declaration_ancestor_iterator(this, name),
        visible_declaration_ancestor_iterator());
  }

  void InsertDeclaration(Declaration const *decl);

  absl::flat_hash_map<std::string_view, std::vector<Declaration::Id const *>>
      decls_;

  // Invokes `f` on each declaration in this and all descendant scopes in the
  // order of occurence.
  void ForEachNonConstantDeclaration(
      std::invocable<Declaration const *> auto &&f) const {
    for (auto p : ordered_non_constant_declarations_) {
      if (auto const *decl = p.get_if<Declaration>()) {
        f(decl);
      } else {
        p.get<Scope>()->ForEachNonConstantDeclaration(f);
      }
    }
  }

  void ForEachNonConstantDeclarationSpan(
      std::invocable<absl::Span<code_location_t const>> auto &&f) const {
    auto start = ordered_non_constant_declarations_.begin();
    auto iter  = std::find_if(
        start, ordered_non_constant_declarations_.end(),
        [](code_location_t loc) -> bool { return loc.get_if<Scope>(); });
    absl::Span<code_location_t const> span(&*start, std::distance(start, iter));
    if (not span.empty()) { f(span); }

    while (iter != ordered_non_constant_declarations_.end()) {
      ASSERT(iter->template get_if<Scope>() != nullptr);
      iter->template get<Scope>()->ForEachNonConstantDeclarationSpan(f);

      auto start = std::next(iter);
      iter       = std::find_if(
          start, ordered_non_constant_declarations_.end(),
          [](code_location_t loc) -> bool { return loc.get_if<Scope>(); });
      absl::Span<code_location_t const> span(&*start,
                                             std::distance(start, iter));
      if (not span.empty()) { f(span); }
    }
  }

  absl::Span<Declaration::Id const *const> VisibleChildren(
      std::string_view name) const {
    if (auto iter = child_decls_.find(name); iter != child_decls_.end()) {
      return iter->second;
    }
    return absl::Span<Declaration::Id const *const>();
  }

  void embed(module::Module *module) { embedded_modules_.insert(module); }

  absl::flat_hash_set<module::Module *> const &embedded_modules() const {
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
  absl::flat_hash_set<module::Module *> embedded_modules_;
  std::vector<Scope *> executable_descendants_;

  // Sequence consisting of pointers to either a declaration or a child scope in
  // the order that they appear.
  std::vector<code_location_t> ordered_non_constant_declarations_;
};

}  // namespace ast

#endif  // ICARUS_AST_SCOPE_H
