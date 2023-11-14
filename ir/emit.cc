#include "ir/emit.h"

#include "common/debug.h"
#include "common/module_id.h"
#include "common/resources.h"
#include "ir/serialize.h"
#include "jasmin/execute.h"
#include "jasmin/instructions/arithmetic.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {
namespace {

void HandleParseTreeNodeModule(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<jasmin::Return>();
  context.pop_function();
}

void HandleParseTreeNodeModuleStart(ParseNodeIndex index,
                                    EmitContext& context) {
  auto& f = context.current_module.initializer();
  context.push_function(f, Scope::Index::Root());
  f.append<jasmin::StackAllocate>(context.current_storage().size().value());
}

void HandleParseTreeNodeBooleanLiteral(ParseNodeIndex index,
                                       EmitContext& context) {
  auto node = context.Node(index);
  NTH_REQUIRE((v.debug), node.token.kind() == Token::Kind::True or
                             node.token.kind() == Token::Kind::False);
  context.current_function().append<jasmin::Push>(node.token.kind() ==
                                                  Token::Kind::True);
}

void HandleParseTreeNodeIntegerLiteral(ParseNodeIndex index,
                                       EmitContext& context) {
  context.current_function().append<jasmin::Push>(
      context.Node(index).token.AsInteger());
}

void HandleParseTreeNodeStringLiteral(ParseNodeIndex index,
                                      EmitContext& context) {
  std::string_view s =
      resources.StringLiteral(context.Node(index).token.AsStringLiteralIndex());
  context.current_function().append<PushStringLiteral>(s.data(), s.size());
}

void HandleParseTreeNodeTypeLiteral(ParseNodeIndex index,
                                    EmitContext& context) {
  auto node = context.Node(index);
  switch (node.token.kind()) {
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  case Token::Kind::kind:                                                      \
    context.current_function().append<PushType>(type::symbol);                 \
    break;
#include "common/language/primitive_types.xmacro.h"
    default: NTH_UNREACHABLE();
  }
}

void HandleParseTreeNodeBuiltinLiteral(ParseNodeIndex index,
                                       EmitContext& context) {
  context.current_function().append<jasmin::Push>(ModuleId::Builtin());
}

void HandleParseTreeNodeScopeStart(ParseNodeIndex, EmitContext&) {}

void StoreStackValue(IrFunction& f, type::ByteWidth offset, type::Type t) {
  type::ByteWidth type_width = type::Contour(t).byte_width();
  type::ByteWidth end        = offset + type_width;
  type::ByteWidth position =
      end.aligned_backward_to(type::Alignment(jasmin::ValueSize));
  if (position != end) {
    f.append<jasmin::StackOffset>(position.value());
    f.append<Store>((end - position).value());
  }
  while (position != offset) {
    position -= type::ByteWidth(jasmin::ValueSize);
    f.append<jasmin::StackOffset>(position.value());
    f.append<Store>(jasmin::ValueSize);
  }
}

void LoadStackValue(IrFunction& f, type::ByteWidth offset, type::Type t) {
  type::ByteWidth type_width = type::Contour(t).byte_width();
  type::ByteWidth end        = offset + type_width;
  while (offset + type::ByteWidth(jasmin::ValueSize) <= end) {
    f.append<jasmin::StackOffset>(offset.value());
    f.append<jasmin::Load>(jasmin::ValueSize);
    offset += type::ByteWidth(jasmin::ValueSize);
  }
  if (offset< end) {
    f.append<jasmin::StackOffset>(offset.value());
    f.append<jasmin::Load>((end - offset).value());
  }
}

Iteration HandleParseTreeNodeFunctionLiteralStart(ParseNodeIndex index,
                                                  EmitContext& context) {
  auto fn_type           = context.QualifiedTypeOf(index).type().AsFunction();
  auto const& parameters = *fn_type.parameters();
  size_t input_size      = 0;
  size_t output_size     = 0;
  type::ByteWidth bytes(0);
  std::vector<type::ByteWidth> storage_offsets;
  storage_offsets.reserve(parameters.size());
  for (auto const& p : parameters) {
    input_size += type::JasminSize(p.type);
    storage_offsets.push_back(bytes);
    auto contour = type::Contour(p.type);
    bytes.align_forward_to(contour.alignment());
    bytes += contour.byte_width();
  }

  for (auto const& t : fn_type.returns()) {
    output_size += type::JasminSize(t);
  }
  context.push_function(
      context.current_module.add_function(input_size, output_size),
      context.Node(index).scope_index);
  auto& f = context.current_function();
  f.append<jasmin::StackAllocate>(context.current_storage().size().value());

  auto storage_iter = storage_offsets.rbegin();
  for (auto iter = parameters.rbegin(); iter != parameters.rend();
       ++storage_iter, ++iter) {
    StoreStackValue(f, *storage_iter, iter->type);
  }

  // TODO: We should be able to jump directly rather than iterate and check.
  while (context.Node(index).kind !=
         ParseNode::Kind::FunctionLiteralSignature) {
    ++index;
  }
  return Iteration::SkipTo(index);
}

void HandleParseTreeNodeFunctionLiteralSignature(ParseNodeIndex index,
                                                 EmitContext& context) {}

void HandleParseTreeNodeDeclaration(ParseNodeIndex index,
                                    EmitContext& context) {
  NTH_REQUIRE(not context.queue.front().declaration_stack.empty());
  auto const& decl_info = context.queue.front().declaration_stack.back();
  if (not decl_info.kind.has_initializer()) {
    NTH_REQUIRE((v.debug), not decl_info.kind.inferred_type());
    if (not decl_info.kind.parameter()) { NTH_UNIMPLEMENTED(); }
  } else if (decl_info.kind.inferred_type()) {
    if (decl_info.kind.constant()) {
      auto& f = context.current_function();
      f.append<jasmin::Return>();
      jasmin::ValueStack value_stack;
      jasmin::Execute(f, value_stack);
      auto const* info = context.scopes.identifier(
          context.current_scope_index(),
          context.Node(decl_info.index).token.Identifier());
      NTH_REQUIRE(info != nullptr);
      context.constants.insert_or_assign(
          context.tree.subtree_range(index),
          EmitContext::ComputedConstants(decl_info.index,
                                         std::move(value_stack),
                                         {info->qualified_type.type()}));
      delete &f;
      context.pop_function();
    } else {
      auto range = context.current_storage().range(index);
      context.current_function().append<jasmin::StackOffset>(
          range.lower_bound().value());
      context.current_function().append<Store>(range.length().value());
    }
  } else {
    // TODO: Cast to declared type
    if (decl_info.kind.constant()) {
      auto& f = context.current_function();
      f.append<jasmin::Return>();
      jasmin::ValueStack value_stack;
      jasmin::Execute(f, value_stack);
      auto const* info = context.scopes.identifier(
          context.current_scope_index(),
          context.Node(decl_info.index).token.Identifier());
      NTH_REQUIRE(info != nullptr);
      context.constants.insert_or_assign(
          context.tree.subtree_range(index),
          EmitContext::ComputedConstants(decl_info.index,
                                         std::move(value_stack),
                                         {info->qualified_type.type()}));
      delete &f;
      context.pop_function();
    } else {
      auto range = context.current_storage().range(index);
      context.current_function().append<jasmin::StackOffset>(
          range.lower_bound().value());
      context.current_function().append<Store>(range.length().value());
    }
  }

  if (context.queue.front().declaration_stack.size() == 1 and
      decl_info.kind.constant()) {
    // This is a top-level declaration, we need to export it.
    // TODO: Actually exporting should not be the default.
    context.declarations_to_export.insert(index);
  }
  context.queue.front().declaration_stack.pop_back();
}

void HandleParseTreeNodeStatement(ParseNodeIndex index, EmitContext& context) {
  switch (
      context.Node(context.tree.first_descendant_index(index)).statement_kind) {
    case ParseNode::StatementKind::Expression: {
      auto iter = context.statement_expression_info.find(index);
      NTH_REQUIRE(iter != context.statement_expression_info.end())
          .Log<"For {}">(context.tree.first_descendant_index(index));
      size_t size_to_drop = iter->second.second;
      if (size_to_drop != 0) {
        context.current_function().append<jasmin::Drop>(size_to_drop);
      }
    } break;
    default: break;
  }

  context.queue.front().value_category_stack.pop_back();
}

void HandleParseTreeNodeStatementSequence(ParseNodeIndex index,
                                          EmitContext& context) {}

Iteration HandleParseTreeNodeIdentifier(ParseNodeIndex index,
                                        EmitContext& context) {
  auto [decl_id_index, decl_index] = context.declarator.at(index);
  // TODO: The declarator that this identifier is mapped to may be a constant we
  // can look up, but it may not be the right one!
  //
  // TODO: We should know a priori if it's a constant or not.
  if (auto const* constant = context.constants.mapped_range(decl_index)) {
    context.Push(constant->second);
    return Iteration::Continue;
  } else if (auto offset = context.current_storage().try_offset(decl_index)) {
    switch (context.queue.front().value_category_stack.back()) {
      case EmitContext::ValueCategory::Value: {
        auto qt = context.QualifiedTypeOf(decl_index);
        LoadStackValue(context.current_function(), *offset, qt.type());
      } break;
      case EmitContext::ValueCategory::Reference:
        context.current_function().append<jasmin::StackOffset>(offset->value());
        break;
    }
    return Iteration::Continue;
  } else {
    context.queue.emplace(context.queue.front()).range =
        context.tree.subtree_range(decl_index);
    return Iteration::PauseRetry;
  }
}

void HandleParseTreeNodeDeclaredIdentifier(ParseNodeIndex index,
                                           EmitContext& context) {}

void HandleParseTreeNodeInfixOperator(ParseNodeIndex index,
                                      EmitContext& context) {}

void HandleParseTreeNodeExpressionPrecedenceGroup(ParseNodeIndex index,
                                                  EmitContext& context) {
  auto iter = context.tree.children(index).begin();
  ++iter;
  auto operator_node = *iter;
  size_t child_count = context.Node(index).child_count;
  switch (operator_node.token.kind()) {
    case Token::Kind::MinusGreater: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<ConstructFunctionType>();
      }
    } break;
    case Token::Kind::Plus: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<jasmin::Add<int64_t>>();
      }
    } break;
    case Token::Kind::Minus: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<jasmin::Subtract<int64_t>>();
      }
    } break;
    case Token::Kind::Percent: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<jasmin::Mod<int64_t>>();
      }
    } break;
    case Token::Kind::Star: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<jasmin::Multiply<int64_t>>();
      }
    } break;
    case Token::Kind::EqualEqual: {
        context.current_function().append<jasmin::Equal<int64_t>>();
    } break;
    case Token::Kind::NotEqual: {
      context.current_function().append<jasmin::Equal<int64_t>>();
      context.current_function().append<jasmin::Not>();
    } break;
    case Token::Kind::Less: {
      context.current_function().append<jasmin::LessThan<int64_t>>();
    } break;
    case Token::Kind::Greater: {
      context.current_function().append<jasmin::Swap>();
      context.current_function().append<jasmin::LessThan<int64_t>>();
    } break;
    case Token::Kind::LessEqual: {
      context.current_function().append<jasmin::Swap>();
      context.current_function().append<jasmin::LessThan<int64_t>>();
      context.current_function().append<jasmin::Not>();
    } break;
    case Token::Kind::GreaterEqual: {
      context.current_function().append<jasmin::LessThan<int64_t>>();
      context.current_function().append<jasmin::Not>();
    } break;
    default: NTH_UNIMPLEMENTED("{}") <<= {operator_node.token};
  }
}

Iteration HandleParseTreeNodeDeclarationStart(ParseNodeIndex index,
                                              EmitContext& context) {
  auto info = context.Node(index).declaration_info;
  auto& d   = context.queue.front().declaration_stack.emplace_back();
  d.kind    = info.kind;
  d.index   = index;
  if (not info.kind.has_initializer()) {
    if (info.kind.constant()) {
      NTH_UNIMPLEMENTED();
    } else {
      // Nothing to do here. The type will have already been calculated.
      return Iteration::Continue;
    }
  } else if (info.kind.inferred_type()) {
    if (info.kind.constant()) {
      // TODO: The return might actually be wider and we need to handle that.
      context.push_function(*new IrFunction(0, 1),
                            context.queue.front().function_stack.back());
      return Iteration::PauseMoveOn;
    } else {
      auto iter = context.tree.child_indices(info.index).begin();
      return Iteration::SkipTo(*++iter + 1);
    }
  } else {
    // TODO: Casting to declared type.
    if (info.kind.constant()) {
      // TODO: The return might actually be wider and we need to handle that.
      context.push_function(*new IrFunction(0, 1),
                            context.queue.front().function_stack.back());
      return Iteration::PauseMoveOn;
    } else {
      auto iter = context.tree.child_indices(info.index).begin();
      return Iteration::SkipTo(*++iter + 1);
    }
  }
}

void HandleParseTreeNodeMemberExpression(ParseNodeIndex index,
                                         EmitContext& context) {
  if (context.QualifiedTypeOf(index - 1).type().kind() ==
      type::Type::Kind::Slice) {
    if (context.Node(index).token.Identifier() == Identifier("count")) {
      context.current_function().append<jasmin::Swap>();
    }
    context.current_function().append<jasmin::Drop>(1);
  } else {
    // TODO: Fix this bug.
    decltype(context.constants.mapped_range(index - 1)) mapped_range = nullptr;
    for (auto const& p : context.constants.mapped_intervals()) {
      if (p.first.contains(index - 1)) {
        mapped_range = &p;
        break;
      }
    }
    // auto const* mapped_range = context.constants.mapped_range(index - 1);
    NTH_REQUIRE((v.harden), mapped_range != nullptr);
    context.current_function().append<jasmin::Drop>(1);

    ModuleId module_id;
    bool successfully_deserialized =
        IcarusDeserializeValue(mapped_range->second.value_span(), module_id);
    NTH_REQUIRE((v.harden), successfully_deserialized);

    auto symbol = context.module(module_id).Lookup(
        context.Node(index).token.Identifier());
    context.Push(symbol.value, symbol.qualified_type.type());
  }
}

void HandleParseTreeNodeCallExpression(ParseNodeIndex index,
                                       EmitContext& context) {
  auto iter = context.rotation_count.find(index);
  NTH_REQUIRE((v.harden), iter != context.rotation_count.end());
  context.current_function().append<Rotate>(iter->second + 1);
  context.current_function().append<jasmin::Call>();
}

void HandleParseTreeNodePointer(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<ConstructPointerType>();
}

void HandleParseTreeNodeBufferPointer(ParseNodeIndex index,
                                      EmitContext& context) {
  context.current_function().append<ConstructBufferPointerType>();
}

void HandleParseTreeNodeInvocationArgumentStart(ParseNodeIndex index,
                                                EmitContext& context) {}

void HandleParseTreeNodeSlice(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<ConstructSliceType>();
}

void HandleParseTreeNodeImport(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<jasmin::Push>(
      context.constants.at(index).value_span()[0]);
}

void HandleParseTreeNodeFunctionTypeParameters(ParseNodeIndex index,
                                               EmitContext& context) {
  context.current_function().append<ConstructParametersType>(
      context.Node(index).child_count);
}

void HandleParseTreeNodeIfStatementTrueBranchStart(ParseNodeIndex index,
                                                   EmitContext& context) {
  context.push_scope(context.Node(index).scope_index);
  context.current_function().append<jasmin::Not>();
  context.queue.front().branches.push_back(
      context.current_function().append_with_placeholders<jasmin::JumpIf>());
}

void HandleParseTreeNodeIfStatementFalseBranchStart(ParseNodeIndex index,
                                                    EmitContext& context) {
  jasmin::OpCodeRange jump = context.queue.front().branches.back();
  context.queue.front().branches.back() =
      context.current_function().append_with_placeholders<jasmin::Jump>();
  jasmin::OpCodeRange land = context.current_function().append<NoOp>();
  context.current_function().set_value(
      jump, 0, jasmin::OpCodeRange::Distance(land, jump));
}

void HandleParseTreeNodeIfStatement(ParseNodeIndex index,
                                    EmitContext& context) {
  jasmin::OpCodeRange jump = context.queue.front().branches.back();
  context.queue.front().branches.pop_back();
  jasmin::OpCodeRange land = context.current_function().append<NoOp>();
  context.current_function().set_value(
      jump, 0, jasmin::OpCodeRange::Distance(land, jump));
  context.pop_scope();
}

void HandleParseTreeNodeStatementStart(ParseNodeIndex index,
                                       EmitContext& context) {
  switch (context.Node(index).statement_kind) {
    case ParseNode::StatementKind::Assignment:
      context.queue.front().value_category_stack.push_back(
          EmitContext::ValueCategory::Reference);
      break;
    default:
      context.queue.front().value_category_stack.push_back(
          EmitContext::ValueCategory::Value);
      break;
  }
}

void HandleParseTreeNodeAssignedValueStart(ParseNodeIndex index,
                                           EmitContext& context) {
  context.queue.front().value_category_stack.back() =
      EmitContext::ValueCategory::Value;
}

void HandleParseTreeNodeAssignment(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<Rotate>(2);
  context.current_function().append<Store>(1);
}

void HandleParseTreeNodeReturn(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<jasmin::Return>();
}

void HandleParseTreeNodeFunctionLiteral(ParseNodeIndex index,
                                        EmitContext& context) {
  jasmin::Value f = &context.current_function();
  context.current_function().append<jasmin::Return>();
  context.pop_function();
  context.Push(std::span(&f, 1), {context.QualifiedTypeOf(index).type()});
}

void HandleParseTreeNodeNoReturns(ParseNodeIndex index, EmitContext& context) {}

template <auto F>
constexpr Iteration Invoke(ParseNodeIndex index, EmitContext& context) {
  constexpr auto return_type = nth::type<
      std::invoke_result_t<decltype(F), ParseNodeIndex, EmitContext&>>;
  if constexpr (return_type == nth::type<Iteration>) {
    return F(index, context);
  } else {
    F(index, context);
    return Iteration::Continue;
  }
}

}  // namespace

void EmitContext::Push(std::span<jasmin::Value const> vs, type::Type t) {
  if (t == type::Type_) {
    NTH_REQUIRE((v.harden), vs.size() == 1);
    current_function().append<PushType>(vs[0].as<type::Type>());
    return;
  }
  switch (t.kind()) {
    case type::Type::Kind::GenericFunction:
    case type::Type::Kind::Function: {
      NTH_REQUIRE((v.harden), vs.size() == 1);
      current_function().append<PushFunction>(vs[0]);
    } break;
    case type::Type::Kind::Slice: {
      NTH_REQUIRE((v.harden), vs.size() == 2);
      if (t == type::Slice(type::Char)) {
        current_function().append<PushStringLiteral>(vs[0].as<char const*>(),
                                                     vs[1].as<size_t>());
      } else {
        NTH_UNIMPLEMENTED();
      }
    } break;
    default: {
      NTH_REQUIRE((v.harden), vs.size() == 1);
      current_function().append<jasmin::Push>(vs[0]);
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

void EmitIr(EmitContext& context) {
  while (not context.queue.empty()) {
    auto [start, end] = context.queue.front().range;
    NTH_LOG((v.when(debug::emit)), "Starting emission of [{}, {}) @ {}") <<=
        {start, end, &context.current_function()};
  emit_constant:
    for (auto const& [original_range, constant] :
         context.constants.mapped_intervals()) {
      // It is important to make a copy of this range because we may end up
      // modifying `constants` and invalidating the reference.
      auto range = original_range;
      if (range.lower_bound() < start) {
        continue;
      } else if (range.lower_bound() == start) {
        if (context.Node(range.upper_bound() - 1).kind !=
            ParseNode::Kind::Declaration) {
          context.Push(constant);
        }
        start = range.upper_bound();
        continue;
      } else if (range.lower_bound() < end) {
        // TODO: Can you avoid the duplication of the macros here? You need this
        // in place so that when you copy the front declaration_stack you get
        // updates from this chunk.
        for (; start < range.lower_bound(); ++start) {
          switch (context.Node(start).kind) {
#define IC_XMACRO_PARSE_NODE(node_kind)                                        \
  case ParseNode::Kind::node_kind: {                                           \
    NTH_LOG((v.when(debug::emit)), "Emit node {} {}") <<= {#node_kind, start}; \
    switch (Iteration it =                                                     \
                Invoke<HandleParseTreeNode##node_kind>(start, context);        \
            it.kind()) {                                                       \
      case Iteration::PauseMoveOn: ++start; [[fallthrough]];                   \
      case Iteration::PauseRetry: goto stop_early;                             \
      case Iteration::Continue: break;                                         \
      case Iteration::Skip: start = it.index() - 1; break;                     \
    }                                                                          \
  } break;
#include "parse/node.xmacro.h"
          }
        }
        goto emit_constant;
      } else {
        break;
      }
    }

    for (; start < end; ++start) {
      switch (context.Node(start).kind) {
#define IC_XMACRO_PARSE_NODE(node_kind)                                        \
  case ParseNode::Kind::node_kind: {                                           \
    NTH_LOG((v.when(debug::emit)), "Emit node {} {}") <<= {#node_kind, start}; \
    switch (Iteration it =                                                     \
                Invoke<HandleParseTreeNode##node_kind>(start, context);        \
            it.kind()) {                                                       \
      case Iteration::PauseMoveOn: ++start; [[fallthrough]];                   \
      case Iteration::PauseRetry: goto stop_early;                             \
      case Iteration::Continue: break;                                         \
      case Iteration::Skip: start = it.index() - 1; break;                     \
    }                                                                          \
  } break;
#include "parse/node.xmacro.h"
      }
    }

    NTH_LOG((v.when(debug::emit)), "Done emitting chunk at {}") <<= {start};
    context.queue.pop();
    continue;

  stop_early:
    NTH_LOG((v.when(debug::emit)), "Stopping early just before {}") <<= {start};
    auto* f    = &context.current_function();
    auto& back = context.queue.emplace(std::move(context.queue.front()));
    back.range = nth::interval(start, back.range.upper_bound());
    context.queue.pop();
    continue;
  }
  NTH_LOG((v.when(debug::emit)), "Done emitting!");
  return;
}

void EmitContext::Evaluate(nth::interval<ParseNodeIndex> subtree,
                           jasmin::ValueStack& value_stack,
                           std::vector<type::Type> types) {
  jasmin::ValueStack vs;
  size_t size = 0;
  for (type::Type t : types) { size += type::JasminSize(t); }
  IrFunction f(0, size);
  queue.push({.range = subtree});
  push_function(f, Scope::Index::Root());

  EmitIr(*this);
  f.append<jasmin::Return>();

  jasmin::Execute(f, vs);
  for (jasmin::Value v : vs) { value_stack.push(v); }
  constants.insert_or_assign(
      subtree, ComputedConstants(subtree.upper_bound() - 1, std::move(vs),
                                 std::move(types)));
}

void SetExported(EmitContext const& context) {
  for (auto index : context.declarations_to_export) {
    auto const& constant = context.constants.at(index);
    auto iter            = context.tree.child_indices(index).begin();
    ++iter;
    std::span types      = constant.types();
    std::span value_span = constant.value_span();
    NTH_REQUIRE((v.harden), types.size() == 1);
    // TODO: This is pretty gross. We can't iterate because the subtree_size is
    // shared in a union with another field we want to use.
    context.current_module.Insert(
        context.Node(*iter - 1).token.Identifier(),
        Module::Entry{.qualified_type = type::QualifiedType::Constant(types[0]),
                      .value          = absl::InlinedVector<jasmin::Value, 2>(
                          value_span.begin(), value_span.end())});
  }
}

}  // namespace ic
