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
  uint32_t payload_value = context.Node(index).token.AsIntegerPayload().value();
  uint64_t value;
  if (payload_value >= Token::IntegerPayload::PayloadLimit) {
    value = resources.integers.from_index(payload_value);
  } else {
    value = payload_value;
  }
  context.function_stack.back()->append<jasmin::Push>(value);
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
    context.function_stack.back()->append<PushType>(type::symbol);             \
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
  auto const & decl_info = context.declaration_stack.back();
  switch (decl_info.kind) {
    case Token::Kind::ColonColonEqual: {
      auto& f = *context.temporary_functions.back();
      f.append<jasmin::Return>();
      jasmin::ValueStack value_stack;
      jasmin::Execute(f, value_stack);
      auto iter = context.identifiers.find(
          context.Node(decl_info.index).token.IdentifierIndex());
      NTH_REQUIRE(iter != context.identifiers.end());

      // TODO: Insert types!
      context.constants.insert_or_assign(
          context.tree.subtree_range(index),
          EmitContext::ComputedConstants(decl_info.index,
                                         std::move(value_stack),
                                         {iter->second.second.type()}));

      NTH_REQUIRE(context.temporary_functions.back().get() ==
                  context.function_stack.back());
      context.temporary_functions.pop_back();
      context.function_stack.pop_back();

    } break;
    default: NTH_UNIMPLEMENTED();
  }
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
  auto decl_index = context.declarator.at(index);
  // TODO: The declarator that this identifier is mapped to may be a constant we
  // can look up, but it may not be the right one!

  auto const& constant = context.constants.at(decl_index);
  context.Push(constant);
}

void HandleParseTreeNodeDeclaredIdentifier(ParseTree::Node::Index index,
                                           EmitContext& context) {
  context.declaration_stack.back().index = index;
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

void HandleParseTreeNodeLet(ParseTree::Node::Index, EmitContext& context) {
  context.declaration_stack.emplace_back();
}

void HandleParseTreeNodeVar(ParseTree::Node::Index, EmitContext& context) {
  context.declaration_stack.emplace_back();
}

void HandleParseTreeNodeColonColonEqual(ParseTree::Node::Index,
                                        EmitContext& context) {
  context.declaration_stack.back().kind = Token::Kind::ColonColonEqual;
  // TODO: The value 1 is potentially wrong here.
  auto* f = context.temporary_functions
                .emplace_back(std::make_unique<IrFunction>(0, 1))
                .get();
  context.function_stack.push_back(f);
}

void HandleParseTreeNodeColonEqual(ParseTree::Node::Index,
                                   EmitContext& context) {
  context.declaration_stack.back().kind = Token::Kind::ColonColonEqual;
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeColonColon(ParseTree::Node::Index,
                                   EmitContext& context) {
  context.declaration_stack.back().kind = Token::Kind::ColonColonEqual;
  // TODO: The value 1 is potentially wrong here.
  auto* f = context.temporary_functions
                .emplace_back(std::make_unique<IrFunction>(0, 1))
                .get();
  context.function_stack.push_back(f);
}

void HandleParseTreeNodeColon(ParseTree::Node::Index, EmitContext& context) {
  context.declaration_stack.back().kind = Token::Kind::ColonColonEqual;
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeExpressionGroup(ParseTree::Node::Index, EmitContext&) {
  // Nothing to do here.
}

void HandleParseTreeNodeMemberExpression(ParseTree::Node::Index index,
                                         EmitContext& context) {
  auto const* mapped_range = context.constants.mapped_range(index - 1);
  NTH_REQUIRE((v.harden), mapped_range != nullptr);
  context.function_stack.back()->append<jasmin::Drop>(1);

  ModuleId module_id;
  bool successfully_deserialized =
      IcarusDeserializeValue(mapped_range->second.value_span(), module_id);
  NTH_REQUIRE((v.harden), successfully_deserialized);

  auto symbol = context.module(module_id).Lookup(
      context.Node(index).token.IdentifierIndex());
  context.Push(symbol.value, symbol.qualified_type.type());
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
    NTH_LOG((v.when(false)), "Emit node {} {} {}") <<=                         \
        {#kind, context.function_stack.size(),                                 \
         context.function_stack.empty() ? nullptr                              \
                                        : context.function_stack.back()};      \
    HandleParseTreeNode##kind(index, context);                                 \
    break;
#include "parser/parse_tree_node_kind.xmacro.h"
    }
  }
}

}  // namespace

void EmitContext::Push(std::span<jasmin::Value const> vs, type::Type t) {
  if (t == type::Type_) {
    NTH_REQUIRE((v.harden), vs.size() == 1);
    function_stack.back()->append<PushType>(vs[0].as<type::Type>());
    return;
  }
  switch (t.kind()) {
    case type::Type::Kind::GenericFunction:
    case type::Type::Kind::Function: {
      NTH_REQUIRE((v.harden), vs.size() == 1);
      function_stack.back()->append<PushFunction>(vs[0]);
    } break;
    case type::Type::Kind::Slice: {
      NTH_REQUIRE((v.harden), vs.size() == 2);
      if (t == type::Slice(type::Char)) {
        function_stack.back()->append<PushStringLiteral>(
            vs[0].as<char const*>(), vs[1].as<size_t>());
      } else {
        NTH_UNIMPLEMENTED();
      }
    } break;
    default: {
      NTH_REQUIRE((v.harden), vs.size() == 1);
      function_stack.back()->append<jasmin::Push>(vs[0]);
    } break;
  }
}

void EmitContext::Push(std::span<jasmin::Value const> vs,
                       std::span<type::Type const> ts) {
  for (type::Type t : ts) {
    size_t width = type::JasminSize(t);
    Push(vs.subspan(0, width), t);
    vs.subspan(width);
  }
}

void EmitContext::Push(EmitContext::ComputedConstants const& c) {
  Push(c.value_span(), c.types());
}

void EmitIr(nth::interval<ParseTree::Node::Index> node_range,
            EmitContext& context) {
  ParseTree::Node::Index start = node_range.lower_bound();
  for (auto const& [range, constant] : context.constants.mapped_intervals()) {
    if (range.lower_bound() < start) { continue; }
    EmitNonConstant(nth::interval(start, range.lower_bound()), context);
    context.Push(constant);
    start = range.upper_bound();
  }
  if (start < node_range.upper_bound()) {
    EmitNonConstant(nth::interval(start, node_range.upper_bound()), context);
  }
  context.function_stack.back()->append<jasmin::Return>();
}

void EmitContext::Evaluate(nth::interval<ParseTree::Node::Index> subtree,
                           jasmin::ValueStack& value_stack,
                           std::vector<type::Type> types) {
  jasmin::ValueStack vs;
  // TODO: The size here is potentially wrong. We should compute it based on
  // `types`.
  IrFunction f(0, 1);
  function_stack.push_back(&f);
  EmitIr(subtree, *this);
  f.append<jasmin::Return>();
  jasmin::Execute(f, vs);
  for (jasmin::Value v : vs) { value_stack.push(v); }
  constants.insert_or_assign(
      subtree, ComputedConstants(subtree.upper_bound() - 1, std::move(vs),
                                 std::move(types)));
  function_stack.pop_back();
}

}  // namespace ic
