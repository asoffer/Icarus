#ifndef ICARUS_PARSER_PARSE_TREE_H
#define ICARUS_PARSER_PARSE_TREE_H

#include <cstdint>
#include <span>
#include <vector>

#include "lexer/token.h"

namespace ic {

struct ParseTree {
  struct Node {
    struct Index {
      private:
        friend ParseTree;
        explicit constexpr Index(uint32_t n) : value_(n) {}
        uint32_t value_;
    };

    enum class Kind {
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind) kind,
#include "parser/parse_tree_node_kind.xmacro.h"
    };
    Kind kind;
    uint32_t subtree_size;
    Token token = Token::Invalid();
  };
  static_assert(sizeof(Node) == 16);

  std::span<Node const> nodes() const { return nodes_; }
  uint32_t size() const { return nodes_.size(); }

  std::span<Node const> subtree(Node::Index node_index) const;

  void append(Node::Kind kind, Token token, int subtree_start);

  void append_leaf(Node::Kind kind, Token token) {
    nodes_.push_back({.kind = kind, .subtree_size = 1, .token = token});
  }

 private:
  std::vector<Node> nodes_;
};

}  // namespace ic

#endif  // ICARUS_PARSER_PARSE_TREE_H
