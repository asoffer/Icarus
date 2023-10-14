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
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  case Token::Kind::kind:                                                      \
    context.function_stack.back()->append<jasmin::Push>(type::symbol);         \
    break;
#include "common/language/primitive_types.xmacro.h"
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
  auto iter = context.tree.children(index).begin();
  ++iter;
  auto node = *iter;
  switch (node.token.kind()) {
    case Token::Kind::MinusGreater: {
      context.function_stack.back()->append<ConstructFunctionType>();
    } break;
    default: NTH_UNIMPLEMENTED();
  }
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
  auto const* mapped_range = context.constants.mapped_range(index);
  NTH_REQUIRE((v.harden), mapped_range != nullptr);
  context.function_stack.back()->append<jasmin::Drop>(1);

  ModuleId module_id;
  bool successfully_deserialized =
      IcarusDeserializeValue(mapped_range->second.value_span(), module_id);
  NTH_REQUIRE((v.harden), successfully_deserialized);

  auto symbol = context.module(module_id).Lookup(
      context.Node(index).token.IdentifierIndex());
  for (jasmin::Value const& v : symbol.value) {
    context.Push(v, symbol.qualified_type.type());
  }
}

void HandleParseTreeNodeCallExpression(ParseTree::Node::Index index,
                                       EmitContext& context) {
  auto iter = context.rotation_count.find(index);
  NTH_REQUIRE((v.harden), iter != context.rotation_count.end());
  context.function_stack.back()->append<Rotate>(iter->second + 1);
  context.function_stack.back()->append<jasmin::Call>();
}

void HandleParseTreeNodeInvocationArgumentStart(ParseTree::Node::Index index,
                                                EmitContext& context) {}

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
    case type::Type::Kind::GenericFunction:
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
  for (auto const& [range, constant] : context.constants.mapped_intervals()) {
    if (range.lower_bound() < start) { continue; }
    EmitNonConstant(nth::interval(start, range.lower_bound()), context);
    // TODO: This type is wrong.
    for (jasmin::Value const& v : constant.value_span()) {
      context.Push(v, type::Bool);
    }
    start = range.upper_bound();
  }
  EmitNonConstant(nth::interval(start, node_range.upper_bound()), context);
  context.function_stack.back()->append<jasmin::Return>();
}

void EmitContext::Evaluate(nth::interval<ParseTree::Node::Index> subtree,
                           jasmin::ValueStack& value_stack) {
  jasmin::ValueStack vs;
  IrFunction f(0, 1);
  function_stack.push_back(&f);
  EmitIr(subtree, *this);
  f.append<jasmin::Return>();
  jasmin::Execute(f, vs);
  for (jasmin::Value v : vs) { value_stack.push(v); }
  constants.insert_or_assign(
      subtree, ComputedConstant(subtree.upper_bound() - 1, std::move(vs)));
  function_stack.pop_back();
}

}  // namespace ic
