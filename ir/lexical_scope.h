#ifndef ICARUS_IR_LEXICAL_SCOPE_H
#define ICARUS_IR_LEXICAL_SCOPE_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "common/identifier.h"
#include "common/strong_identifier_type.h"
#include "nth/debug/debug.h"
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

  Index parent() const;

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

  explicit LexicalScope(Index parent_index) : parent_(parent_index) {}

  absl::flat_hash_map<Identifier, DeclarationInfo> identifiers_;

  Index parent_;
};

struct LexicalScopeTree {
  LexicalScope::Index insert_child(LexicalScope::Index parent_index);

  LexicalScope &root();
  LexicalScope const &root() const;

  LexicalScope &operator[](LexicalScope::Index index);
  LexicalScope const &operator[](LexicalScope::Index index) const;

  LexicalScope::DeclarationInfo const *identifier(LexicalScope::Index index,
                                                  Identifier id) const;

  LexicalScopeTree()
      : scopes_(1, LexicalScope(LexicalScope::Index::Invalid())) {}

 private:
  std::vector<LexicalScope> scopes_;
};

}  // namespace ic

#endif  // ICARUS_IR_LEXICAL_SCOPE_H
