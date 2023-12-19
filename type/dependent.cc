#include "type/dependent.h"

namespace ic::type::internal_dependent {

Term Term::DeBruijnIndex(uint32_t index) {
  Term term;
  term.nodes_.push_back({.kind         = Node::Kind::DeBruijnIndex,
                         .subtree_size = 1,
                         .payload      = {.index = index}});
  return term;
}

Term Term::Type(type::Type t) {
  Term term;
  term.nodes_.push_back(
      {.kind = Node::Kind::Type, .subtree_size = 1, .payload = {.type = t}});
  return term;
}

Term Term::Function(Term const &type, Term term) {
  term.nodes_.insert(term.nodes_.end(), type.nodes_.begin(), type.nodes_.end());
  term.nodes_.push_back({
      .kind         = Node::Kind::Function,
      .subtree_size = static_cast<uint32_t>(term.nodes_.size() + 1),
  });
  return term;
}

}  // namespace ic::type::internal_dependent
