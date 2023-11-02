#include "ir/emit.h"

#include "common/debug.h"
#include "common/module_id.h"
#include "common/resources.h"
#include "ir/serialize.h"
#include "jasmin/execute.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {
namespace {

enum class Iteration {
  Continue,
  PauseRetry,
  PauseMoveOn,
};

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

void HandleParseTreeNodeFunctionStart(ParseNodeIndex index,
                                      EmitContext& context) {
  context.current_function().append<jasmin::StackAllocate>(
      context.current_storage().size().value());
}

void HandleParseTreeNodeDeclaration(ParseNodeIndex index,
                                    EmitContext& context) {
  NTH_REQUIRE(not context.queue.front().declaration_stack.empty());
  auto const& decl_info = context.queue.front().declaration_stack.back();
  switch (decl_info.kind) {
    case Token::Kind::ColonEqual: {
      context.current_function().append<jasmin::StackOffset>(
          context.current_storage().offset(index).value());

      auto qt_iter = context.statement_qualified_type.find(index);
      NTH_REQUIRE((v.debug), qt_iter != context.statement_qualified_type.end());
      context.current_function().append<Store>(
          type::Contour(qt_iter->second.type()).byte_width().value());
    } break;
    case Token::Kind::ColonColonEqual: {
      auto& f = context.current_function();
      f.append<jasmin::Return>();
      jasmin::ValueStack value_stack;
      jasmin::Execute(f, value_stack);
      auto const* info = context.scopes.identifier(
          context.current_scope_index(),
          context.Node(decl_info.index).token.Identifier());
      NTH_REQUIRE(info != nullptr);
      auto vs_iter    = value_stack.begin();
      auto& prev_func = *(vs_iter++)->as<IrFunction*>();
      jasmin::ValueStack vs;
      for (; vs_iter != value_stack.end(); ++vs_iter) { vs.push(*vs_iter); }
      context.constants.insert_or_assign(
          context.tree.subtree_range(index),
          EmitContext::ComputedConstants(decl_info.index, std::move(vs),
                                         {info->qualified_type.type()}));
      delete &f;
      context.set_current_function(prev_func);
    } break;
    case Token::Kind::Colon: {
      NTH_UNIMPLEMENTED("Store in a stack-allocated variable.");
    } break;
    default: NTH_UNIMPLEMENTED("{}") <<= {decl_info.kind};
  }

  if (context.queue.front().declaration_stack.size() == 1 and
      (decl_info.kind == Token::Kind::ColonColon or
       decl_info.kind == Token::Kind::ColonColonEqual)) {
    // This is a top-level declaration, we need to export it.
    // TODO: Actually exporting should not be the default.
    context.declarations_to_export.insert(index);
  }
  context.queue.front().declaration_stack.pop_back();
}

void HandleParseTreeNodeStatement(ParseNodeIndex index, EmitContext& context) {
  auto iter = context.statement_qualified_type.find(index);
  NTH_REQUIRE(iter != context.statement_qualified_type.end());
  context.current_function().append<jasmin::Drop>(
      type::JasminSize(iter->second.type()));
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
    context.current_function().append<jasmin::StackOffset>(offset->value());
    auto qt_iter = context.statement_qualified_type.find(decl_index);
    NTH_REQUIRE((v.debug), qt_iter != context.statement_qualified_type.end());
    context.current_function().append<jasmin::Load>(
        type::Contour(qt_iter->second.type()).byte_width().value());
    return Iteration::Continue;
  } else {
    context.queue.emplace(context.queue.front()).range =
        context.tree.subtree_range(decl_index);
    return Iteration::PauseRetry;
  }
}

void HandleParseTreeNodeDeclaredIdentifier(ParseNodeIndex index,
                                           EmitContext& context) {
  context.queue.front().declaration_stack.back().index = index;
}

void HandleParseTreeNodeInfixOperator(ParseNodeIndex index,
                                      EmitContext& context) {}

void HandleParseTreeNodeExpressionPrecedenceGroup(ParseNodeIndex index,
                                                  EmitContext& context) {
  auto iter = context.tree.children(index).begin();
  ++iter;
  auto node = *iter;
  switch (node.token.kind()) {
    case Token::Kind::MinusGreater: {
      context.current_function().append<ConstructFunctionType>();
    } break;
    default: NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeLet(ParseNodeIndex, EmitContext& context) {
  context.queue.front().declaration_stack.emplace_back();
}

void HandleParseTreeNodeVar(ParseNodeIndex, EmitContext& context) {
  context.queue.front().declaration_stack.emplace_back();
}

Iteration HandleParseTreeNodeColonColonEqual(ParseNodeIndex index,
                                             EmitContext& context) {
  context.queue.front().declaration_stack.back().kind =
      Token::Kind::ColonColonEqual;
  // We need somewhere to stash the value of the current function so we can
  // resume it. To do this, we push the current function as the very first
  // argument (and therefore first return value).
  // TODO: Note 2 here because it's 1 for the current function and 1 for the
  // return. The return might actually be wider and we need to handle that.
  auto& f = *new IrFunction(0, 2);
  f.append<PushFunction>(&context.current_function());
  context.set_current_function(f);
  return Iteration::PauseMoveOn;
}

void HandleParseTreeNodeColonEqual(ParseNodeIndex, EmitContext& context) {
  context.queue.front().declaration_stack.back().kind = Token::Kind::ColonEqual;
}

void HandleParseTreeNodeColonColon(ParseNodeIndex, EmitContext& context) {
  context.queue.front().declaration_stack.back().kind = Token::Kind::ColonColon;
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeColon(ParseNodeIndex, EmitContext& context) {
  context.queue.front().declaration_stack.back().kind = Token::Kind::Colon;
  // Nothing to do here. The type will have already been calculated.
}

void HandleParseTreeNodeExpressionGroup(ParseNodeIndex, EmitContext&) {
  // Nothing to do here.
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

void HandleParseTreeNodeImport(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<jasmin::Push>(
      context.constants.at(index).value_span()[0]);
}

void HandleParseTreeNodeFunctionTypeParameters(ParseNodeIndex index,
                                               EmitContext& context) {
  context.current_function().append<ConstructParametersType>(
      context.Node(index).child_count);
}

void HandleParseTreeNodeBeginIfStatementTrueBranch(ParseNodeIndex index,
                                                   EmitContext& context) {
  context.push_scope(context.Node(index).scope_index);
  context.current_function().append<jasmin::Not>();
  context.queue.front().branches.push_back(
      context.current_function().append_with_placeholders<jasmin::JumpIf>());
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
  auto& f = context.current_function();
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
#define IC_XMACRO_PARSE_NODE(kind)                                             \
  case ParseNode::Kind::kind: {                                                \
    NTH_LOG((v.when(debug::emit)), "Emit node {} {}") <<= {#kind, start};      \
    switch (Invoke<HandleParseTreeNode##kind>(start, context)) {               \
      case Iteration::PauseMoveOn: ++start; [[fallthrough]];                   \
      case Iteration::PauseRetry: goto stop_early;                             \
      case Iteration::Continue: break;                                         \
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
#define IC_XMACRO_PARSE_NODE(kind)                                             \
  case ParseNode::Kind::kind: {                                                \
    NTH_LOG((v.when(debug::emit)), "Emit node {} {}") <<= {#kind, start};      \
    switch (Invoke<HandleParseTreeNode##kind>(start, context)) {               \
      case Iteration::PauseMoveOn: ++start; [[fallthrough]];                   \
      case Iteration::PauseRetry: goto stop_early;                             \
      case Iteration::Continue: break;                                         \
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
  f.append<jasmin::Return>();
  NTH_LOG((v.when(debug::emit)), "Done emitting!");
  return;
}

void EmitContext::Evaluate(nth::interval<ParseNodeIndex> subtree,
                           jasmin::ValueStack& value_stack,
                           std::vector<type::Type> types) {
  jasmin::ValueStack vs;
  // TODO: The size here is potentially wrong. We should compute it based on
  // `types`.
  IrFunction f(0, 1);
  queue.push({
      .function       = &f,
      .range          = subtree,
      .function_stack = {Scope::Index::Root()},
  });

  EmitIr(*this);

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
