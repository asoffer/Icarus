#include "ir/lexical_scope.h"

#include "nth/debug/debug.h"

namespace ic {

LexicalScope::Index LexicalScope::Index::Root() { return Index(0); }

LexicalScope::Index LexicalScope::Index::Invalid() {
  return Index(std::numeric_limits<Index::underlying_type>::max());
}

LexicalScope::Index LexicalScopeTree::insert_child(
    LexicalScope::Index parent_index) {
  LexicalScope::Index index(scopes_.size());
  scopes_.push_back(LexicalScope(index.value() - parent_index.value()));
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

}  // namespace ic
