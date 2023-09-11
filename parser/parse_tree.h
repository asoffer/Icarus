#ifndef ICARUS_PARSER_PARSE_TREE_H
#define ICARUS_PARSER_PARSE_TREE_H

#include <cstdint>
#include <span>
#include <vector>

#include "lexer/token.h"
#include "nth/strings/interpolate.h"
#include "nth/utility/iterator_range.h"

namespace ic {

struct ParseTree {
  struct Node {
    struct Index {
      explicit constexpr Index(uint32_t n) : value_(n) {}


      friend constexpr Index operator+(Index i, int32_t n) {
        return Index(i.value_ + n);
      }

      friend constexpr Index operator-(Index i, int32_t n) {
        return Index(i.value_ - n);
      }

     private:
      friend ParseTree;
      uint32_t value_;
    };

    enum class Kind : uint8_t {
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind) kind,
#include "parser/parse_tree_node_kind.xmacro.h"
    };

    friend void NthPrint(auto& p, auto& f, Node const &n) {
      nth::Interpolate<"({}, size={}, {})">(p, f, n.kind, n.subtree_size,
                                            n.token);
    }

    template <typename H>
    friend H AbslHashValue(H h, Node n) {
      return H::combine(std::move(h), n.kind, n.subtree_size, n.token);
    }
    friend bool operator==(Node const&, Node const &) = default;
    friend bool operator!=(Node const&, Node const &) = default;

    Kind kind;
    // Note: This field may not be populated on all node kinds.
    int16_t child_count = -1;
    uint32_t subtree_size;
    Token token = Token::Invalid();
  };
  static_assert(sizeof(Node) == 16);

  std::span<Node const> nodes() const { return nodes_; }
  uint32_t size() const { return nodes_.size(); }

  std::span<Node const> subtree(Node::Index node_index) const;

  Node &operator[](Node::Index node_index);
  Node const &operator[](Node::Index node_index) const;

  struct sibling_iterator {
    sibling_iterator &operator++() {
      node_ -= node_->subtree_size;
      return *this;
    }

    sibling_iterator operator++(int) {
      auto copy = *this;
      ++*this;
      return copy;
    }

    sibling_iterator &operator--() {
      node_ += node_->subtree_size;
      return *this;
    }

    sibling_iterator operator--(int) {
      auto copy = *this;
      --*this;
      return copy;
    }

    Node const &operator*() { return *node_; }
    Node const *operator->() { return node_; }

    friend auto operator<=>(sibling_iterator, sibling_iterator) = default;

   private:
    friend ParseTree;
    sibling_iterator(Node const *node) : node_(node) {}
    Node const *node_;
  };

  auto children(Node::Index node_index) const {
    auto *p = &nodes_[node_index.value_];
    return nth::iterator_range(sibling_iterator(p - 1),
                               sibling_iterator(p - p->subtree_size));
  }

  void append(Node::Kind kind, Token token, int subtree_start);
  void set_back_child_count();

  void append_leaf(Node::Kind kind, Token token) {
    nodes_.push_back({.kind = kind, .subtree_size = 1, .token = token});
  }

 private:
  std::vector<Node> nodes_;
};

void NthPrint(auto &p, auto &, ParseTree::Node::Kind k) {
  static constexpr std::array KindStrings{
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind) "p." #kind,
#include "parser/parse_tree_node_kind.xmacro.h"
  };
  p.write(KindStrings[static_cast<uint8_t>(k)]);
}

}  // namespace ic

#endif  // ICARUS_PARSER_PARSE_TREE_H
