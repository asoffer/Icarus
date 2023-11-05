#include "parse/tree.h"

#include "nth/debug/debug.h"

namespace ic {

nth::interval<ParseNodeIndex> ParseTree::subtree_range(
    ParseNodeIndex node_index) const {
  NTH_REQUIRE((v.harden), node_index.value() < nodes_.size());
  return nth::interval(first_descendant_index(node_index), node_index + 1);
}

ParseTree::sibling_index_range ParseTree::child_indices(
    ParseNodeIndex node_index) const {
  auto* p = &nodes_[node_index.value()];
  return nth::iterator_range(
      sibling_index_iterator(&nodes_[0], p - 1),
      sibling_index_iterator(&nodes_[0], p - p->subtree_size));
}

ParseTree::sibling_range ParseTree::children(ParseNodeIndex node_index) const {
  auto* p = &nodes_[node_index.value()];
  return nth::iterator_range(sibling_iterator(p - 1),
                             sibling_iterator(p - p->subtree_size));
}

ParseNodeIndex ParseTree::first_descendant_index(
    ParseNodeIndex node_index) const {
  return node_index - nodes_[node_index.value()].subtree_size + 1;
}

std::span<ParseNode const> ParseTree::subtree(ParseNodeIndex node_index) const {
  auto [lower, upper] = subtree_range(node_index);
  return std::span<ParseNode const>(&nodes_[lower.value()],
                                    &nodes_[upper.value()]);
}

void ParseTree::append(ParseNode::Kind kind, Token token, int subtree_start) {
  uint32_t size = static_cast<uint32_t>(nodes_.size() - subtree_start + 1);
  nodes_.push_back({.kind = kind, .subtree_size = size, .token = token});
}

void ParseTree::set_back_child_count() {
  int16_t count = 0;
  for (auto const& unused : children(ParseNodeIndex(nodes_.size() - 1))) {
    ++count;
  }
  nodes_.back().child_count = count;
}

ParseNode& ParseTree::operator[](ParseNodeIndex node_index) {
  NTH_REQUIRE((v.harden), node_index.value() < size());

  return nodes_[node_index.value()];
}

ParseNode const& ParseTree::operator[](ParseNodeIndex node_index) const {
  NTH_REQUIRE((v.harden), node_index.value() < size());
  return nodes_[node_index.value()];
}

}  // namespace ic
