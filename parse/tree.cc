#include "parse/tree.h"

#include "nth/debug/debug.h"

namespace ic {

nth::interval<ParseNode::Index> ParseTree::subtree_range(
    ParseNode::Index node_index) const {
  NTH_REQUIRE((v.harden), node_index.value() < nodes_.size());
  auto const& node = nodes_[node_index.value()];
  return nth::interval(node_index - node.subtree_size + 1, node_index + 1);
}

ParseTree::sibling_index_range ParseTree::child_indices(
    ParseNode::Index node_index) const {
  auto* p = &nodes_[node_index.value()];
  return nth::iterator_range(
      sibling_index_iterator(&nodes_[0], p - 1),
      sibling_index_iterator(&nodes_[0], p - p->subtree_size));
}

ParseTree::sibling_range ParseTree::children(
    ParseNode::Index node_index) const {
  auto* p = &nodes_[node_index.value()];
  return nth::iterator_range(sibling_iterator(p - 1),
                             sibling_iterator(p - p->subtree_size));
}

std::span<ParseNode const> ParseTree::subtree(
    ParseNode::Index node_index) const {
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
  for (auto const& unused : children(ParseNode::Index(nodes_.size() - 1))) {
    ++count;
  }
  nodes_.back().child_count = count;
}

ParseNode& ParseTree::operator[](ParseNode::Index node_index) {
  NTH_REQUIRE((v.harden), node_index.value() < size());

  return nodes_[node_index.value()];
}

ParseNode const& ParseTree::operator[](ParseNode::Index node_index) const {
  NTH_REQUIRE((v.harden), node_index.value() < size());
  return nodes_[node_index.value()];
}

}  // namespace ic
