#include "ir/scope.h"

#include "nth/debug/debug.h"

namespace ic {

Scope::Index Scope::Index::Root() { return Index(0); }

Scope::Index Scope::Index::Invalid() {
  return Index(std::numeric_limits<Index::underlying_type>::max());
}

Scope::Index Scope::parent() const { return parent_; }

Scope::DeclarationInfo const *Scope::identifier(Identifier id) const {
  auto iter = identifiers_.find(id);
  if (iter == identifiers_.end()) { return nullptr; }
  return &iter->second;
}

Scope::Index ScopeTree::insert_child(Scope::Index parent_index) {
  Scope::Index index(scopes_.size());
  scopes_.push_back(Scope(parent_index));
  return index;
}

Scope &ScopeTree::root() { return scopes_[0]; }
Scope const &ScopeTree::root() const { return scopes_[0]; }

Scope &ScopeTree::operator[](Scope::Index index) {
  NTH_REQUIRE(index.value() < scopes_.size());
  return scopes_[index.value()];
}

Scope const &ScopeTree::operator[](Scope::Index index) const {
  NTH_REQUIRE(index.value() < scopes_.size());
  return scopes_[index.value()];
}

Scope::DeclarationInfo const *ScopeTree::identifier(Scope::Index index,
                                                    Identifier id) const {
  while (index != Scope::Index::Invalid()) {
    auto const &scope = scopes_[index.value()];
    auto const *info  = scope.identifier(id);
    if (info) { return info; }
    index = scope.parent();
  }
  return nullptr;
}

}  // namespace ic
