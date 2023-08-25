#include "ir/ir.h"

#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"
#include "type/type.h"

namespace ic {
namespace {

void HandleParseTreeNodeBooleanLiteral(ParseTree::Node node,
                                       IrContext& context) {
  context.type_stack.push_back(type::Bool);
}

void HandleParseTreeNodeIntegerLiteral(ParseTree::Node node,
                                       IrContext& context) {
  context.type_stack.push_back(type::Integer);
}

void HandleParseTreeNodeTypeLiteral(ParseTree::Node node, IrContext& context) {
  context.type_stack.push_back(type::Type_);
}

void HandleParseTreeNodeDeclaration(ParseTree::Node node, IrContext& context) {
  context.identifiers.emplace(node.token.IdentifierIndex(),
                              context.type_stack.back());
  context.type_stack.pop_back();
}

void HandleParseTreeNodeStatementSequence(ParseTree::Node node,
                                          IrContext& context) {
  context.emit.statement_type.emplace(node, context.type_stack.back());
}

}  // namespace

void ProcessIr(std::span<ParseTree::Node const> nodes, IrContext& context) {
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
}

}  // namespace ic
