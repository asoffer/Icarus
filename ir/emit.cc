#include "ir/emit.h"

#include "common/resources.h"
#include "ir/module_id.h"
#include "jasmin/execute.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {
namespace {

void HandleParseTreeNodeBooleanLiteral(ParseTree::Node::Index index,
                                       EmitContext& context) {
  auto node = context.Node(index);
  NTH_REQUIRE((v.debug), node.token.kind() == Token::Kind::True or
                             node.token.kind() == Token::Kind::False);
  context.function_stack.back()->append<jasmin::Push>(node.token.kind() ==
                                                      Token::Kind::True);
}

void HandleParseTreeNodeIntegerLiteral(ParseTree::Node::Index index,
                                       EmitContext& context) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeStringLiteral(ParseTree::Node::Index index,
                                      EmitContext& context) {
  std::string_view s =
      resources.StringLiteral(context.Node(index).token.AsStringLiteralIndex());
  context.function_stack.back()->append<PushStringLiteral>(s.data(), s.size());
}

void HandleParseTreeNodeTypeLiteral(ParseTree::Node::Index index,
                                    EmitContext& context) {
  auto node = context.Node(index);
  switch (node.token.kind()) {
    case Token::Kind::Bool:
      context.function_stack.back()->append<jasmin::Push>(type::Bool);
      break;
    default: NTH_UNREACHABLE();
  }
}

void HandleParseTreeNodeBuiltinLiteral(ParseTree::Node::Index index,
                                       EmitContext& context) {
  context.function_stack.back()->append<jasmin::Push>(ModuleId::Builtin());
}

void HandleParseTreeNodeDeclaration(ParseTree::Node::Index index,
                                    EmitContext& context) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeStatementSequence(ParseTree::Node::Index index,
                                          EmitContext& context) {
  auto node = context.Node(index);
  auto iter = context.statement_qualified_type.find(node);
  NTH_REQUIRE(iter != context.statement_qualified_type.end());
  context.function_stack.back()->append<jasmin::Drop>(
      type::JasminSize(iter->second.type()));
}

void HandleParseTreeNodeIdentifier(ParseTree::Node::Index index,
                                   EmitContext& context) {
  auto node = context.Node(index);
  NTH_UNIMPLEMENTED("{}") <<= {node};
}

void HandleParseTreeNodeDeclaredIdentifier(ParseTree::Node::Index index,
                                           EmitContext& context) {
  auto node = context.Node(index);
  NTH_UNIMPLEMENTED("{}") <<= {node};
}

void HandleParseTreeNodeInfixOperator(ParseTree::Node::Index index,
                                      EmitContext& context) {
  auto node = context.Node(index);
  context.operator_stack.push_back(node.token.kind());
}

void HandleParseTreeNodeExpressionPrecedenceGroup(ParseTree::Node::Index index,
                                                  EmitContext& context) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeLet(ParseTree::Node::Index, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeVar(ParseTree::Node::Index, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeColonColonEqual(ParseTree::Node::Index, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeColonEqual(ParseTree::Node::Index, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeColonColon(ParseTree::Node::Index, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeColon(ParseTree::Node::Index, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeExpressionGroup(ParseTree::Node::Index, EmitContext&) {
  // Nothing to do here.
}

void HandleParseTreeNodeMemberExpression(ParseTree::Node::Index index,
                                         EmitContext& context) {
  // TODO: Once we have interval_map this will be easier.
  jasmin::ValueStack const* vs = nullptr;
  for (auto const& [range, value_stack] : context.constants) {
    if (range.upper_bound() == index) {
      vs = &value_stack;
      break;
    }
  }
  NTH_REQUIRE((v.harden), vs != nullptr);

  ModuleId module_id;
  bool successfully_deserialized =
      IcarusDeserializeValue(std::span(vs->begin(), vs->end()), module_id);
  NTH_REQUIRE((v.harden), successfully_deserialized);

  auto symbol = context.module(module_id).Lookup(
      context.Node(index).token.IdentifierIndex());
  for (jasmin::Value const& v : symbol.value) {
    context.Push(v, symbol.qualified_type.type());
  }
}

void HandleParseTreeNodeCallExpression(ParseTree::Node::Index,
                                       EmitContext& context) {
  context.function_stack.back()->append<jasmin::Call>();
}

void EmitNonConstant(nth::interval<ParseTree::Node::Index> node_range,
                     EmitContext& context) {
  if (node_range.empty()) { return; }
  auto* node = &context.tree[node_range.lower_bound()];
  for (auto index = node_range.lower_bound(); index < node_range.upper_bound();
       ++index, ++node) {
    switch (node->kind) {
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind)                                   \
  case ParseTree::Node::Kind::kind:                                            \
    NTH_LOG((v.when(false)), "Emit node {}") <<= {#kind};                      \
    HandleParseTreeNode##kind(index, context);                                 \
    break;
#include "parser/parse_tree_node_kind.xmacro.h"
    }
  }
}

}  // namespace

void EmitContext::Push(jasmin::Value v, type::Type t) {
  switch (t.kind()) {
    case type::Type::Kind::Function: {
      function_stack.back()->append<PushFunction>(v);
    } break;
    default: {
      function_stack.back()->append<jasmin::Push>(v);
    } break;
  }
}

void EmitIr(nth::interval<ParseTree::Node::Index> node_range, EmitContext& context) {
  ParseTree::Node::Index start = node_range.lower_bound();
  for (auto const& [range, value_stack] : context.constants) {
    EmitNonConstant(nth::interval(start, range.lower_bound()), context);
    // TODO: This type is wrong.
    for (jasmin::Value const& v : value_stack) { context.Push(v, type::Bool); }
    start = range.upper_bound();
  }
  EmitNonConstant(nth::interval(start, node_range.upper_bound()), context);
  context.function_stack.back()->append<jasmin::Return>();
}

void Evaluate(nth::interval<ParseTree::Node::Index> subtree,
              ParseTree const& tree,
              DependentModules const& modules NTH_ATTRIBUTE(lifetimebound),
              jasmin::ValueStack& value_stack) {
  IrFunction f(0, 1);
  EmitContext context(tree, modules, f);
  EmitIr(subtree, context);
  context.function_stack.back()->append<jasmin::Return>();
  jasmin::Execute(f, value_stack);
}

}  // namespace ic
