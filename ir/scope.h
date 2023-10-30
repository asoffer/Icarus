#ifndef ICARUS_IR_SCOPE_H
#define ICARUS_IR_SCOPE_H

#include <vector>

#include "absl/container/flat_hash_map.h"
#include "common/identifier.h"
#include "common/strong_identifier_type.h"
#include "parser/parse_tree.h"
#include "type/type.h"

namespace ic {

struct Scope {
  Scope() = delete;

  struct Index : StrongIdentifierType<Index, uint32_t> {
    using StrongIdentifierType::StrongIdentifierType;

    static Index Root();
    static Index Invalid();
  };

  Index parent() const;

  struct DeclarationInfo {
    ParseTree::Node::Index declaration;
    ParseTree::Node::Index identifier;
    type::QualifiedType qualified_type;
  };

 private:
  friend struct ScopeTree;

  explicit Scope(Index parent_index) : parent_(parent_index) {}

  absl::flat_hash_map<Identifier, DeclarationInfo> identifiers_;

  Index parent_;
};

struct ScopeTree {
  Scope::Index insert_child(Scope::Index parent_index);

  Scope &root();
  Scope const &root() const;

  Scope &operator[](Scope::Index index);
  Scope const &operator[](Scope::Index index) const;

  ScopeTree() : scopes_(1, Scope(Scope::Index::Invalid())) {}

 private:
  std::vector<Scope> scopes_;
};

}  // namespace ic

#endif  // ICARUS_IR_SCOPE_H
