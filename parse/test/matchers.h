#ifndef ICARUS_PARSE_TEST_MATCHERS_H
#define ICARUS_PARSE_TEST_MATCHERS_H

#include <algorithm>
#include <vector>

#include "parse/test/tree_node_ref.h"

namespace ic {

#define IC_XMACRO_PARSE_NODE(node)                                             \
  inline constexpr auto node = nth::debug::MakeProperty<#node>(                \
      [](auto const &expr, auto const &...children) {                          \
        using std::begin;                                                      \
        using std::end;                                                        \
        auto &&value = nth::debug::EvaluateTraced(expr);                       \
        auto v       = value.children();                                       \
        std::vector<ParseNodeIndex> child_vector;                              \
        for (auto const &element : v) { child_vector.push_back(element); }     \
        if (value.tree[value.index].kind != ParseNode::Kind::node) {           \
          return false;                                                        \
        }                                                                      \
        if (sizeof...(children) != child_vector.size()) { return false; }      \
        std::reverse(child_vector.begin(), child_vector.end());                \
        auto iter = child_vector.begin();                                      \
        auto e    = child_vector.end();                                        \
        return (nth::debug::Matches(                                           \
                    children,                                                  \
                    TreeNodeRef{.tree = value.tree, .index = *iter++}) and     \
                ...);                                                          \
      });
#include "parse/node.xmacro.h"

}  // namespace ic

#endif // ICARUS_PARSE_TEST_MATCHERS_H
