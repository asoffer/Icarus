#ifndef ICARUS_PARSE_NODE_H
#define ICARUS_PARSE_NODE_H

#include "ir/scope.h"
#include "lexer/token.h"
#include "nth/strings/interpolate.h"
#include "parse/node_index.h"

namespace ic {

// Represents a node in the parse tree. 
struct ParseNode {
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

  enum class StatementKind : uint8_t {
    Unknown,
    Declaration,
    Expression,
    Assignment
  };
  uint32_t subtree_size = 1;
  union {
    struct {
    } unused = {};
    ParseNodeIndex corresponding_statement_sequence;
    ParseNodeIndex declaration;
    Scope::Index scope_index;
    StatementKind statement_kind;
  };
  Token token = Token::Invalid();
};
static_assert(sizeof(ParseNode) == 20);

void NthPrint(auto &p, auto &, ParseNode::Kind k) {
  static constexpr std::array KindStrings{
#define IC_XMACRO_PARSE_NODE(kind) "p." #kind,
#include "parse/node.xmacro.h"
  };
  p.write(KindStrings[static_cast<uint8_t>(k)]);
}

}  // namespace ic

#endif  // ICARUS_PARSE_NODE_H
