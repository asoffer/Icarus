#include "ir/emit.h"

#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {
namespace {

void HandleParseTreeNodeBooleanLiteral(ParseTree::Node node,
                                       EmitContext& context) {
  NTH_ASSERT((v.debug), node.token.kind() == Token::Kind::True or
                            node.token.kind() == Token::Kind::False);
  context.module.initializer.append<jasmin::Push>(node.token.kind() ==
                                                  Token::Kind::True);
}

void HandleParseTreeNodeIntegerLiteral(ParseTree::Node node,
                                       EmitContext& context) {
  // TODO: Choose the count based on the type.
  context.module.initializer.append<jasmin::Drop>(1);
}

void HandleParseTreeNodeDeclaration(ParseTree::Node node,
                                    EmitContext& context) {
}

void HandleParseTreeNodeStatementSequence(ParseTree::Node node,
                                          EmitContext& context) {}

}  // namespace

void EmitIr(std::span<ParseTree::Node const> nodes, EmitContext& context) {
  for (auto const& node : nodes) {
    switch (node.kind) {
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind)                                   \
  case ParseTree::Node::Kind::kind:                                            \
    NTH_LOG((v.always), "Parse node {}") <<= {#kind};                          \
    HandleParseTreeNode##kind(node, context);                                  \
    break;
#include "parser/parse_tree_node_kind.xmacro.h"
    }
  }
  context.module.initializer.append<jasmin::Return>();
}

}  // namespace ic
