#ifndef ICARUS_PARSER_PARSE_TREE_H
#define ICARUS_PARSER_PARSE_TREE_H

#include <cstdint>
#include <span>
#include <vector>

#include "lexer/token.h"
#include "nth/container/interval.h"
#include "nth/strings/interpolate.h"
#include "nth/utility/iterator_range.h"

namespace ic {

template <typename T, typename RepType, typename DiffType>
struct StrongIndexType {
  using underlying_type = RepType;
  using difference_type = DiffType;

  explicit constexpr StrongIndexType() requires(
      std::is_constructible_v<underlying_type>) = default;

  explicit constexpr StrongIndexType(underlying_type const &value)
      : value_(value) {}
  explicit constexpr StrongIndexType(underlying_type &&value)
      : value_(std::move(value)) {}

  friend auto operator<=>(StrongIndexType, StrongIndexType) = default;

  template <typename H>
  friend H AbslHashValue(H h, StrongIndexType n) {
    return H::combine(std::move(h), n.value_);
  }

  underlying_type const &value() const & { return value_; }
  underlying_type &&value() && { return std::move(value_); }

  constexpr T &operator+=(difference_type const &rhs) {
    value_ += rhs;
    return static_cast<T &>(*this);
  }

  constexpr T &operator+=(difference_type &&rhs) {
    value_ += std::move(rhs);
    return static_cast<T &>(*this);
  }

  constexpr T &operator-=(difference_type const &rhs) {
    value_ -= rhs;
    return static_cast<T &>(*this);
  }

  constexpr T &operator-=(difference_type &&rhs) {
    value_ -= std::move(rhs);
    return static_cast<T &>(*this);
  }

  constexpr T &operator++() {
    ++value_;
    return static_cast<T &>(*this);
  }

  constexpr T operator++(int) {
    auto copy = *this;
    ++value_;
    return copy;
  }

  constexpr T &operator--() {
    --value_;
    return static_cast<T &>(*this);
  }

  constexpr T operator--(int) {
    auto copy = *this;
    --value_;
    return copy;
  }

  friend constexpr T operator+(T const &lhs, difference_type const &rhs) {
    return T(lhs.value_ + rhs);
  }

  friend constexpr T operator+(T const &lhs, difference_type &&rhs) {
    return T(lhs.value_ + std::move(rhs));
  }

  friend constexpr T operator+(T &&lhs, difference_type const &rhs) {
    return T(std::move(lhs).value_ + rhs);
  }

  friend constexpr T operator+(T &&lhs, difference_type &&rhs) {
    return T(std::move(lhs).value_ + std::move(rhs));
  }

  friend constexpr T operator-(T const &lhs, difference_type const &rhs) {
    return T(lhs.value_ - rhs);
  }

  friend constexpr T operator-(T const &lhs, difference_type &&rhs) {
    return T(lhs.value_ - std::move(rhs));
  }

  friend constexpr T operator-(T &&lhs, difference_type const &rhs) {
    return T(std::move(lhs).value_ - rhs);
  }

  friend constexpr T operator-(T &&lhs, difference_type &&rhs) {
    return T(std::move(lhs).value_ - std::move(rhs));
  }

  friend constexpr T operator-(T const &lhs, T const &rhs) {
    return T(lhs.value_ - rhs.value_);
  }

  friend constexpr T operator-(T const &lhs, T &&rhs) {
    return T(lhs.value_ - std::move(rhs).value_);
  }

  friend constexpr T operator-(T &&lhs, T const &rhs) {
    return T(std::move(lhs).value_ - rhs.value_);
  }

  friend constexpr T operator-(T &&lhs, T &&rhs) {
    return T(std::move(lhs).value_ - std::move(rhs).value_);
  }

  friend void NthPrint(auto &p, auto &f, T const &t) {
    p.write(nth::type<T>.name());
    p.write("{");
    f(p, t.value_);
    p.write("}");
  }

 private:
  underlying_type value_;
};

}  // namespace ic

namespace std {

template <typename T>
requires(std::is_base_of_v<
         ::ic::StrongIndexType<std::remove_cv_t<T>,
                               typename std::remove_cv_t<T>::underlying_type,
                               typename std::remove_cv_t<T>::difference_type>,
         std::remove_cv_t<T>>) struct numeric_limits<T>
    : private numeric_limits<typename std::remove_cv_t<T>::underlying_type> {
 private:
  using base_type =
      numeric_limits<typename std::remove_cv_t<T>::underlying_type>;

 public:
  using base_type::digits10;
  using base_type::has_denorm;
  using base_type::has_denorm_loss;
  using base_type::has_infinity;
  using base_type::has_quiet_NaN;
  using base_type::has_signaling_NaN;
  using base_type::is_bounded;
  using base_type::is_exact;
  using base_type::is_iec559;
  using base_type::is_integer;
  using base_type::is_signed;
  using base_type::is_specialized;
  using base_type::max_digits10;
  using base_type::max_exponent;
  using base_type::max_exponent10;
  using base_type::min_exponent;
  using base_type::min_exponent10;
  using base_type::radix;
  using base_type::round_style;
  using base_type::tinyness_before;
  using base_type::traps;
  static constexpr bool is_modulo = false;
  static constexpr std::remove_cv_t<T> min() {
    return std::remove_cv_t<T>{base_type::min()};
  }
  static constexpr std::remove_cv_t<T> max() {
    return std::remove_cv_t<T>{base_type::max()};
  }
  static constexpr std::remove_cv_t<T> lowest() {
    return std::remove_cv_t<T>{base_type::lowest()};
  }
  static constexpr std::remove_cv_t<T> epsilon() {
    return std::remove_cv_t<T>{base_type::epsilon()};
  }
  static constexpr std::remove_cv_t<T> round_error() {
    return std::remove_cv_t<T>{base_type::round_error()};
  }
  static constexpr std::remove_cv_t<T> infinity() {
    return std::remove_cv_t<T>{base_type::infinity()};
  }
  static constexpr std::remove_cv_t<T> quiet_NaN() {
    return std::remove_cv_t<T>{base_type::quiet_NaN()};
  }
  static constexpr std::remove_cv_t<T> signalling_NaN() {
    return std::remove_cv_t<T>{base_type::signalling_NaN()};
  }
  static constexpr std::remove_cv_t<T> denorm_min() {
    return std::remove_cv_t<T>{base_type::denorm_min()};
  }
};

}  // namespace std

namespace ic {
struct ParseTree {
  struct Node {
    struct Index : StrongIndexType<Index, uint32_t, int32_t> {
      using StrongIndexType::StrongIndexType;
      static constexpr Index Invalid() {
        return Index(
            std::numeric_limits<StrongIndexType::underlying_type>::max());
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
