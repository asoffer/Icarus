#include "ir/emit.h"

#include "ir/module_id.h"
#include "jasmin/execute.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {
namespace {

void HandleParseTreeNodeBooleanLiteral(ParseTree::Node node,
                                       EmitContext& context) {
  NTH_REQUIRE((v.debug), node.token.kind() == Token::Kind::True or
                             node.token.kind() == Token::Kind::False);
  context.function_stack.back()->append<jasmin::Push>(node.token.kind() ==
                                                      Token::Kind::True);
}

void HandleParseTreeNodeIntegerLiteral(ParseTree::Node node,
                                       EmitContext& context) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeTypeLiteral(ParseTree::Node node,
                                    EmitContext& context) {
  switch (node.token.kind()) {
    case Token::Kind::Bool:
      context.function_stack.back()->append<jasmin::Push>(type::Bool);
      break;
    default: NTH_UNREACHABLE();
  }
}

void HandleParseTreeNodeDeclaration(ParseTree::Node node,
                                    EmitContext& context) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeStatementSequence(ParseTree::Node node,
                                          EmitContext& context) {
  auto iter = context.statement_type.find(node);
  NTH_REQUIRE(iter != context.statement_type.end());
  context.function_stack.back()->append<jasmin::Drop>(type::Size(iter->second));
}

void HandleParseTreeNodeIdentifier(ParseTree::Node node, EmitContext& context) {
  NTH_UNIMPLEMENTED("{}") <<= {node};
}

void HandleParseTreeNodeInfixOperator(ParseTree::Node node,
                                      EmitContext& context) {
  context.operator_stack.push_back(node.token.kind());
}

void HandleParseTreeNodeExpressionPrecedenceGroup(ParseTree::Node node,
                                                  EmitContext& context) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeExpressionGroup(ParseTree::Node node,
                                        EmitContext& context) {
  // Nothing to do here.
}

void HandleParseTreeNodeBuiltin(ParseTree::Node index, EmitContext& context) {
  context.function_stack.back()->append<jasmin::Push>(ModuleId::Builtin());
}

void HandleParseTreeNodeCallArgumentsStart(ParseTree::Node index,
                                           EmitContext& context) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeCallExpression(ParseTree::Node index,
                                       EmitContext& context) {
  NTH_UNIMPLEMENTED();
}

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
  context.function_stack.back()->append<jasmin::Return>();
}

void EmitIr(std::span<ParseTree::Node const> nodes, EmitContext& context);

void Evaluate(std::span<ParseTree::Node const> subtree,
              jasmin::ValueStack& value_stack) {
  IrFunction f(0, 1);
  EmitContext context(f);
  EmitIr(subtree, context);
  context.function_stack.back()->append<jasmin::Return>();
  jasmin::Execute(f, value_stack);
}

}  // namespace ic
