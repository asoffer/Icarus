#ifndef ICARUS_TOOLCHAIN_PARSER_PARSE_TREE_H
#define ICARUS_TOOLCHAIN_PARSER_PARSE_TREE_H

#include <cstdint>
#include <span>
#include <vector>

#include "toolchain/lexer/token.h"

namespace ic {

struct ParseTree {
  struct Node {
    enum class Kind {
      StatementSequence,
      Declaration,
      Identifier,
      IntegerLiteral
    };
    Kind kind;
    Token token;
    uint32_t subtree_size;
  };

  std::span<Node const> nodes() const { return nodes_; }
  uint32_t size() const { return nodes_.size(); }

  void append(Node::Kind kind, Token token, int subtree_start) {
    nodes_.push_back({
        .kind  = kind,
        .token = token,
        .subtree_size =
            static_cast<uint32_t>(nodes_.size() - subtree_start + 1),
    });
  }

  void append_leaf(Node::Kind kind, Token token) {
    nodes_.push_back({.kind = kind, .token = token, .subtree_size = 1});
  }

 private:
  std::vector<Node> nodes_;
};

}  // namespace ic

#endif  // ICARUS_TOOLCHAIN_PARSER_PARSE_TREE_H
