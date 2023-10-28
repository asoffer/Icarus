#ifndef ICARUS_PARSER_PARSE_TREE_H
#define ICARUS_PARSER_PARSE_TREE_H

#include <cstdint>
#include <span>
#include <vector>

#include "common/strong_index_type.h"
#include "lexer/token.h"
#include "nth/container/interval.h"
#include "nth/strings/interpolate.h"
#include "nth/utility/iterator_range.h"

namespace ic {
struct ParseTree {
  struct Node {
    struct Index : StrongIndexType<Index, uint32_t, int32_t> {
      using StrongIndexType::StrongIndexType;
      static constexpr Index Invalid() {
        return Index(
            std::numeric_limits<StrongIndexType::underlying_type>::max());
      }

      friend void NthPrint(auto &p, auto &f, Index const &i) {
        p.write("Index{");
        f(p, i.value());
        p.write("}");
      }
    };

    enum class Kind : uint8_t {
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind) kind,
#include "parser/parse_tree_node_kind.xmacro.h"
    };

    friend void NthPrint(auto& p, auto& f, Node const &n) {
      nth::Interpolate<"({}, size={}, {})">(p, f, n.kind, n.subtree_size,
                                            n.token);
    }

    Kind kind;
    // Note: This field may not be populated on all node kinds.
    int16_t child_count = -1;

    union {
      uint32_t subtree_size;
      Index next_sibling_index;
      Index declaration;
    };
    Token token = Token::Invalid();
  };
  static_assert(sizeof(Node) == 16);

  std::span<Node const> nodes() const { return nodes_; }
  nth::interval<Node::Index> node_range() const {
    return nth::interval(Node::Index{0}, Node::Index(nodes_.size()));
  }
  uint32_t size() const { return nodes_.size(); }

  std::span<Node const> subtree(Node::Index node_index) const;
  nth::interval<Node::Index> subtree_range(Node::Index node_index) const;

  Node &operator[](Node::Index node_index);
  Node const &operator[](Node::Index node_index) const;

  Node &back() { return nodes_.back(); }
  Node const &back() const { return nodes_.back(); }

  struct sibling_iterator_base {
    sibling_iterator_base &operator--() {
      node_ += node_->subtree_size;
      return *this;
    }

    sibling_iterator_base operator--(int) {
      auto copy = *this;
      --*this;
      return copy;
    }

    friend auto operator<=>(sibling_iterator_base,
                            sibling_iterator_base) = default;

    protected:
     void increment() { node_ -= node_->subtree_size; }

    private:
     friend ParseTree;
     explicit sibling_iterator_base(Node const *node) : node_(node) {}
     Node const *node_;
  };

  struct sibling_index_iterator : sibling_iterator_base {
    Node::Index operator*() const { return Node::Index{node_ - start_}; }

    sibling_index_iterator &operator++() {
      increment();
      return *this;
    }

    sibling_index_iterator operator++(int) {
      auto copy = *this;
      ++*this;
      return copy;
    }

   private:
    friend ParseTree;
    explicit sibling_index_iterator(Node const *start, Node const *node)
        : sibling_iterator_base(node), start_(start) {}
    Node const * start_;
  };

  struct sibling_iterator : sibling_iterator_base {
    Node const &operator*() { return *node_; }
    Node const *operator->() { return node_; }

    sibling_iterator &operator++() {
      increment();
      return *this;
    }

    sibling_iterator operator++(int) {
      auto copy = *this;
      ++*this;
      return copy;
    }

   private:
    friend ParseTree;
    using sibling_iterator_base::sibling_iterator_base;
  };

  auto child_indices(Node::Index node_index) const {
    auto *p = &nodes_[node_index.value()];
    return nth::iterator_range(
        sibling_index_iterator(&nodes_[0], p - 1),
        sibling_index_iterator(&nodes_[0], p - p->subtree_size));
  }

  auto children(Node::Index node_index) const {
    auto *p = &nodes_[node_index.value()];
    return nth::iterator_range(sibling_iterator(p - 1),
                               sibling_iterator(p - p->subtree_size));
  }

  void append(Node::Kind kind, Token token, int subtree_start);

  void append_leaf(Node::Kind kind, Token token) {
    nodes_.push_back({.kind = kind, .subtree_size = 1, .token = token});
  }

  void set_back_child_count();

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
