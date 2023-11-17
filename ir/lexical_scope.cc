#include "ir/lexical_scope.h"

#include "nth/debug/debug.h"

namespace ic {

LexicalScope::Index LexicalScope::Index::Root() { return Index(0); }

LexicalScope::Index LexicalScope::Index::Invalid() {
  return Index(std::numeric_limits<Index::underlying_type>::max());
}

LexicalScope::Index LexicalScope::parent() const { return parent_; }

LexicalScope::DeclarationInfo const *LexicalScope::identifier(
    Identifier id) const {
  auto iter = identifiers_.find(id);
  if (iter == identifiers_.end()) { return nullptr; }
  return &iter->second;
}

LexicalScope::Index LexicalScopeTree::insert_child(
    LexicalScope::Index parent_index) {
  LexicalScope::Index index(scopes_.size());
  scopes_.push_back(LexicalScope(parent_index));
  return index;
}

LexicalScope &LexicalScopeTree::root() { return scopes_[0]; }
LexicalScope const &LexicalScopeTree::root() const { return scopes_[0]; }

LexicalScope &LexicalScopeTree::operator[](LexicalScope::Index index) {
  NTH_REQUIRE(index.value() < scopes_.size());
  return scopes_[index.value()];
}

LexicalScope const &LexicalScopeTree::operator[](
    LexicalScope::Index index) const {
  NTH_REQUIRE(index.value() < scopes_.size());
  return scopes_[index.value()];
}

LexicalScope::DeclarationInfo const *LexicalScopeTree::identifier(
    LexicalScope::Index index, Identifier id) const {
  while (index != LexicalScope::Index::Invalid()) {
    auto const &scope = scopes_[index.value()];
    auto const *info  = scope.identifier(id);
    if (info) { return info; }
    index = scope.parent();
  }
  return nullptr;
}

}  // namespace ic
