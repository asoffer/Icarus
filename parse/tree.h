#ifndef ICARUS_PARSE_TREE_H
#define ICARUS_PARSE_TREE_H

#include <cstdint>
#include <span>
#include <vector>

#include "lexer/token.h"
#include "nth/container/interval.h"
#include "nth/utility/iterator_range.h"
#include "parse/node.h"
#include "parse/node_index.h"

namespace ic {

struct ParseTree {
 private:
  struct sibling_iterator_base;
  struct sibling_index_iterator;
  struct sibling_iterator;

  using sibling_range = nth::iterator_range<sibling_iterator, sibling_iterator>;
  using sibling_index_range =
      nth::iterator_range<sibling_index_iterator, sibling_index_iterator>;

 public:
  std::span<ParseNode const> nodes() const { return nodes_; }
  nth::interval<ParseNodeIndex> node_range() const {
    return nth::interval(ParseNodeIndex{0}, ParseNodeIndex(nodes_.size()));
  }
  uint32_t size() const { return nodes_.size(); }

  std::span<ParseNode const> subtree(ParseNodeIndex node_index) const;
  nth::interval<ParseNodeIndex> subtree_range(ParseNodeIndex node_index) const;

  ParseNode &operator[](ParseNodeIndex node_index);
  ParseNode const &operator[](ParseNodeIndex node_index) const;

  ParseNode &back() { return nodes_.back(); }
  ParseNode const &back() const { return nodes_.back(); }

  sibling_range children(ParseNodeIndex node_index) const;
  sibling_index_range child_indices(ParseNodeIndex node_index) const;

  void append(ParseNode::Kind kind, Token token, int subtree_start);

  void append_leaf(ParseNode::Kind kind, Token token) {
    nodes_.push_back({.kind = kind, .subtree_size = 1, .token = token});
  }

  void set_back_child_count();

 private:
  std::vector<ParseNode> nodes_;
};

struct ParseTree::sibling_iterator_base {
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
  explicit sibling_iterator_base(ParseNode const *node) : node_(node) {}
  ParseNode const *node_;
};

struct ParseTree::sibling_index_iterator : ParseTree::sibling_iterator_base {
  ParseNodeIndex operator*() const { return ParseNodeIndex{node_ - start_}; }

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
  explicit sibling_index_iterator(ParseNode const *start, ParseNode const *node)
      : sibling_iterator_base(node), start_(start) {}
  ParseNode const *start_;
};

struct ParseTree::sibling_iterator : ParseTree::sibling_iterator_base {
  ParseNode const &operator*() { return *node_; }
  ParseNode const *operator->() { return node_; }

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

}  // namespace ic

#endif  // ICARUS_PARSE_TREE_H
