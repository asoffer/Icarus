#include "parser/parse_tree.h"

#include "nth/debug/debug.h"

namespace ic {

std::span<ParseTree::Node const> ParseTree::subtree(
    Node::Index node_index) const {
  NTH_ASSERT((v.debug), node_index.value_ < nodes_.size());
  auto const& node = nodes_[node_index.value_];
  return std::span<Node const>(&node - node.subtree_size + 1, &node + 1);
}

void ParseTree::append(Node::Kind kind, Token token, int subtree_start) {
  uint32_t size = static_cast<uint32_t>(nodes_.size() - subtree_start + 1);
  nodes_.push_back({.kind = kind, .subtree_size = size, .token = token});
}

}  // namespace ic
