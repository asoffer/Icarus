#ifndef ICARUS_IR_LEXICAL_SCOPE_H
#define ICARUS_IR_LEXICAL_SCOPE_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "common/identifier.h"
#include "common/strong_identifier_type.h"
#include "nth/debug/debug.h"
#include "nth/utility/iterator_range.h"
#include "parse/node_index.h"
#include "type/type.h"

namespace ic {

struct LexicalScope {
  LexicalScope() = delete;

  struct Index : StrongIdentifierType<Index, uint32_t> {
    using StrongIdentifierType::StrongIdentifierType;

    static Index Root();
    static Index Invalid();
  };

  struct DeclarationInfo {
    ParseNodeIndex declaration;
    ParseNodeIndex identifier;
    type::QualifiedType qualified_type;
  };

  DeclarationInfo const *identifier(Identifier id) const;

  void insert_identifier(Identifier id, DeclarationInfo const &info) {
    [[maybe_unused]] auto [iter, inserted] = identifiers_.emplace(id, info);
    NTH_REQUIRE((v.harden), inserted);
  }

 private:
  friend struct LexicalScopeTree;

  explicit LexicalScope(uint32_t distance_to_parent)
      : parent_distance_(distance_to_parent) {}

  absl::flat_hash_map<Identifier, DeclarationInfo> identifiers_;

  uint32_t parent_distance_;
};

struct LexicalScopeTree {
  LexicalScope::Index insert_child(LexicalScope::Index parent_index);

  LexicalScope &root();
  LexicalScope const &root() const;

 private:
  struct ancestor_iterator_base {
    friend bool operator==(ancestor_iterator_base,
                           ancestor_iterator_base) = default;

   protected:
    explicit ancestor_iterator_base(LexicalScope const *scope)
        : scope_(scope) {}

    LexicalScope const *scope_;
  };

 public:
  template <typename S>
  struct ancestor_iterator_impl : ancestor_iterator_base {
    ancestor_iterator_impl &operator++() {
      LexicalScopeTree::ReplaceWithParent(scope_);
      return *this;
    }

    ancestor_iterator_impl operator++(int) {
      auto copy = *this;
      ++*this;
      return copy;
    }

    S &operator*() const { return *const_cast<S *>(scope_); }
    S *operator->() const { return const_cast<S *>(scope_); }

   private:
    friend struct LexicalScopeTree;
    using ancestor_iterator_base::ancestor_iterator_base;
  };

  using ancestor_iterator = ancestor_iterator_impl<LexicalScope>;
  using const_ancestor_iterator = ancestor_iterator_impl<LexicalScope const>;

  auto ancestors(LexicalScope::Index index) const {
    return nth::iterator_range(
        const_ancestor_iterator(scopes_.data() + index.value()),
        const_ancestor_iterator(scopes_.data() - 1));
  }

  auto ancestors(LexicalScope::Index index) {
    return nth::iterator_range(
        ancestor_iterator(scopes_.data() + index.value()),
        ancestor_iterator(scopes_.data()));
  }

  LexicalScope &operator[](LexicalScope::Index index);
  LexicalScope const &operator[](LexicalScope::Index index) const;

  LexicalScope::DeclarationInfo const *identifier(LexicalScope::Index index,
                                                  Identifier id) const;

  LexicalScopeTree() : scopes_(1, LexicalScope(1)) {}

 private:
  template <typename>
  friend struct ancestor_iterator_impl;

  static void ReplaceWithParent(LexicalScope const *&scope) {
    scope -= scope->parent_distance_;
  }

  std::vector<LexicalScope> scopes_;
};

}  // namespace ic

#endif  // ICARUS_IR_LEXICAL_SCOPE_H
