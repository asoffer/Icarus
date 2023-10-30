#include "parse/parse_tree.h"

#include "nth/debug/debug.h"

namespace ic {

nth::interval<ParseTree::Node::Index> ParseTree::subtree_range(
    Node::Index node_index) const {
  NTH_REQUIRE((v.harden), node_index.value() < nodes_.size());
  auto const& node = nodes_[node_index.value()];
  return nth::interval(node_index - node.subtree_size + 1, node_index + 1);
}

std::span<ParseTree::Node const> ParseTree::subtree(
    Node::Index node_index) const {
  auto [lower, upper] = subtree_range(node_index);
  return std::span<Node const>(&nodes_[lower.value()], &nodes_[upper.value()]);
}

void ParseTree::append(Node::Kind kind, Token token, int subtree_start) {
  uint32_t size = static_cast<uint32_t>(nodes_.size() - subtree_start + 1);
  nodes_.push_back({.kind = kind, .subtree_size = size, .token = token});
}

void ParseTree::set_back_child_count() {
  int16_t count = 0;
  for (auto const& unused : children(Node::Index(nodes_.size() - 1))) {
    ++count;
  }
  nodes_.back().child_count = count;
}

ParseTree::Node& ParseTree::operator[](ParseTree::Node::Index node_index) {
  NTH_REQUIRE((v.harden), node_index.value() < size());

  return nodes_[node_index.value()];
}

ParseTree::Node const& ParseTree::operator[](
    ParseTree::Node::Index node_index) const {
  NTH_REQUIRE((v.harden), node_index.value() < size());
  return nodes_[node_index.value()];
}

}  // namespace ic
