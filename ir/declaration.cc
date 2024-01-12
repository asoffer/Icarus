#include "ir/declaration.h"

#include <optional>
#include <vector>

#include "common/identifier.h"
#include "common/string.h"
#include "nth/container/stack.h"
#include "nth/utility/iterator_range.h"
#include "parse/node.h"

namespace ic {
namespace {

template <typename NodeType>
struct tree {
  using node_type = NodeType;

  size_t insert_child(size_t parent_index) {
    nodes_.emplace_back(
        std::make_pair(nodes_.size() - parent_index, node_type()));
    return nodes_.size() - 1;
  }

 private:
  struct ancestor_iterator_base {
    friend bool operator==(ancestor_iterator_base,
                           ancestor_iterator_base) = default;

   protected:
    explicit ancestor_iterator_base(std::pair<size_t, node_type> const *node) : node_(node) {}

    std::pair<size_t, node_type> const *node_;
  };

 public:
  template <typename S>
  struct ancestor_iterator_impl : ancestor_iterator_base {
    ancestor_iterator_impl &operator++() {
      tree::ReplaceWithParent(this->node_);
      return *this;
    }

    ancestor_iterator_impl operator++(int) {
      auto copy = *this;
      ++*this;
      return copy;
    }

    S &operator*() const {
      return const_cast<std::pair<size_t, S> *>(this->node_)->second;
    }
    S *operator->() const {
      return &const_cast<std::pair<size_t, S> *>(this->node_)->second;
    }

   private:
    friend tree;
    using ancestor_iterator_base::ancestor_iterator_base;
  };

  using ancestor_iterator       = ancestor_iterator_impl<node_type>;
  using const_ancestor_iterator = ancestor_iterator_impl<node_type const>;

  auto ancestors(size_t index) const {
    return nth::iterator_range(const_ancestor_iterator(nodes_.data() + index),
                               const_ancestor_iterator(nodes_.data() - 1));
  }

  auto ancestors(size_t index) {
    return nth::iterator_range(ancestor_iterator(nodes_.data() + index),
                               ancestor_iterator(nodes_.data()));
  }

  node_type &operator[](size_t index) { return nodes_[index].second; }
  node_type const &operator[](size_t index) const { return nodes_[index].second; }

  size_t size() const { return nodes_.size(); }

  tree() : nodes_(1, std::make_pair(1, node_type())) {}

 private:
  template <typename>
  friend struct ancestor_iterator_impl;

  static void ReplaceWithParent(std::pair<size_t, node_type> const *&node) {
    node -= node->first;
  }

  std::vector<std::pair<size_t, node_type>> nodes_;
};

struct Entry {
  std::vector<ParseNodeIndex> identifiers;
  std::optional<ParseNodeIndex> declaration;
};

}  // namespace

bool AssignDeclarationsToIdentifiers(ParseTree &tree,
                                     diag::DiagnosticConsumer &diag) {
  bool error = false;
  ::ic::tree<absl::flat_hash_map<Identifier, Entry>> entry_tree;
  nth::stack<size_t> indices{0};

  absl::flat_hash_map<Identifier, Entry> *ptr = nullptr;
  auto [start, end]                           = tree.node_range();
  for (auto i = start; i < end; ++i) {
    switch (tree[i].kind) {
      case ParseNode::Kind::FunctionLiteralStart:
      case ParseNode::Kind::ScopeStart:
        indices.push(entry_tree.insert_child(indices.top()));
        ptr = &entry_tree[indices.top()];
        break;
      case ParseNode::Kind::StatementSequence:
      case ParseNode::Kind::FunctionLiteral:
      case ParseNode::Kind::Scope:
        indices.pop();
        ptr = &entry_tree[indices.top()];
        break;
      case ParseNode::Kind::DeclaredIdentifier: {
        auto &decl_id = (*ptr)[tree[i].token.Identifier()].declaration;
        if (decl_id) {
          error = true;
          diag.Consume({
              diag::Header(diag::MessageKind::Error),
              diag::Text(
                  "Symbol has been declared previously in the same scope."),
              diag::SourceQuote(tree[*decl_id].token),
              diag::SourceQuote(tree[i].token),
          });
        } else {
          decl_id = i;
        }
      } break;
      case ParseNode::Kind::Identifier:
        (*ptr)[tree[i].token.Identifier()].identifiers.push_back(i);
        break;
      default: break;
    }
  }
  NTH_REQUIRE((v.harden), indices.size() == 1);
  for (size_t i = 0; i < entry_tree.size(); ++i) {
    for (auto const &[key, entry] : entry_tree[i]) {
      bool found = false;
      for (auto a : entry_tree.ancestors(i)) {
        auto iter = a.find(key);
        if (iter == a.end()) { continue; }
        auto &decl_entry = iter->second;
        if (not decl_entry.declaration) { continue; }
        if (found) {
          error = true;
          diag.Consume({
              diag::Header(diag::MessageKind::Error),
              diag::Text(
                  InterpolateString<
                      "Symbol `{}` has been declared in a parent scope.">(key)),
              diag::SourceQuote(tree[*decl_entry.declaration].token),
          });
        } else {
          found = true;
          for (auto index : entry.identifiers) {
            tree[index].corresponding_declaration = *decl_entry.declaration;
          }
        }
      }
      if (not found) {
          error = true;
          diag.Consume({
              diag::Header(diag::MessageKind::Error),
              diag::Text(
                  InterpolateString<"Symbol `{}` has not been declared.">(key)),
          });
      }
    }
  }
  return not error;
}

}  // namespace ic
