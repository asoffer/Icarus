#ifndef ICARUS_PARSE_TEST_TREE_NODE_REF_H
#define ICARUS_PARSE_TEST_TREE_NODE_REF_H

#include <utility>
#include <variant>

#include "nth/base/attributes.h"
#include "nth/container/stack.h"
#include "nth/meta/type.h"
#include "parse/node.h"
#include "parse/node_index.h"
#include "parse/tree.h"

namespace ic {

struct TreeNodeRef {
  ParseTree const &tree;
  ParseNodeIndex index;
  auto children() const { return tree.child_indices(index); }

  friend void NthPrint(auto &p, auto &f, TreeNodeRef const &t) {
    struct Indent {
      int n;
    };
    using Action = std::variant<ParseNodeIndex, Indent>;
    nth::stack<Action> actions;
    actions.push(t.index);
    p.write("\n");
    int indentation = 4;
    while (not actions.empty()) {
      auto action = std::move(actions.top());
      actions.pop();
      std::visit(
          [&](auto a) {
            constexpr auto type = nth::type<decltype(a)>;
            if constexpr (type == nth::type<ParseNodeIndex>) {
              p.write(indentation, ' ');
              f(p, t.tree[a].kind);
              p.write("\n");
              actions.push(Indent{.n = -2});
              for (auto index : t.tree.child_indices(a)) {
                actions.push(index);
              }
              actions.push(Indent{.n = 2});
            } else {
              indentation += a.n;
            }
          },
          action);
    }
  }
};

TreeNodeRef FromRoot(ParseTree const &tree NTH_ATTRIBUTE(lifetimebound)) {
  return {
      .tree  = tree,
      .index = ParseNodeIndex{tree.size() - 1},
  };
}
}  // namespace ic

#endif // ICARUS_PARSE_TEST_TREE_NODE_REF_H
