#ifndef ICARUS_PARSE_NODE_H
#define ICARUS_PARSE_NODE_H

#include "common/strong_index_type.h"
#include "lexer/token.h"
#include "nth/strings/interpolate.h"

namespace ic {

// Represents a node in the parse tree. 
struct ParseNode {
  // Represents the location of a `ParseNode` within the `ParseTree`. A
  // `ParseTree` is layed out linearly in memory so that traversing nodes in
  // order represents a post-order traversal of the tree. See "parse/tree.h" for
  // more details.
  struct Index : StrongIndexType<Index, uint32_t, int32_t> {
    using StrongIndexType::StrongIndexType;

    static Index Invalid();

    friend void NthPrint(auto &p, auto &f, Index const &i) {
      p.write("Index{");
      f(p, i.value());
      p.write("}");
    }
  };

  // Represents a category describing this parse tree node. Examples include
  // `IfStatement`, `Declaration` and `Import`. Note that not all categories
  // represent traditional parse-tree nodes. Because traversal is not done
  // recursively, some nodes exist purely to indicate actions that need to be
  // taken before entering a subtree. For example, the
  // `BeginIfStatementTrueBranch` indicates traversal is about to enter the true
  // branch of an if-statement and provides an opportunity to update information
  // during traversal such as the current scope.
  enum class Kind : uint8_t {
#define IC_XMACRO_PARSE_NODE(kind) kind,
#include "parse/node.xmacro.h"
  };

  friend void NthPrint(auto &p, auto &f, ParseNode const &n) {
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
static_assert(sizeof(ParseNode) == 16);

void NthPrint(auto &p, auto &, ParseNode::Kind k) {
  static constexpr std::array KindStrings{
#define IC_XMACRO_PARSE_NODE(kind) "p." #kind,
#include "parse/node.xmacro.h"
  };
  p.write(KindStrings[static_cast<uint8_t>(k)]);
}

}  // namespace ic

#endif  // ICARUS_PARSE_NODE_H
