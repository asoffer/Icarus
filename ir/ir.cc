#include "ir/ir.h"

#include <optional>
#include <span>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "common/debug.h"
#include "common/module_id.h"
#include "common/resources.h"
#include "common/string.h"
#include "ir/scope.h"
#include "jasmin/execute.h"
#include "jasmin/value_stack.h"
#include "nth/debug/debug.h"
#include "parse/node_index.h"
#include "parse/tree.h"
#include "type/type.h"

namespace ic {
namespace {

#define IC_PROPAGATE_ERRORS(context, node)                                     \
  do {                                                                         \
    auto&& c = (context);                                                      \
    auto&& n = (node);                                                         \
    for (auto const* p =                                                       \
             &c.type_stack()[c.type_stack().size() - n.child_count];           \
         p != &c.type_stack().back(); ++p) {                                   \
      if (p->type() == type::Error) {                                          \
        c.type_stack().resize(c.type_stack().size() - n.child_count + 1);      \
        c.type_stack().back() = type::QualifiedType::Constant(type::Error);    \
        return;                                                                \
      }                                                                        \
    }                                                                          \
  } while (false);

template <typename T>
type::Type FromConstant() {
  auto t = nth::type<T>;
  if constexpr (t == nth::type<type::Type>) {
    return type::Type_;
  } else if constexpr (t == nth::type<ModuleId>) {
    return type::Module;
  } else if constexpr (t == nth::type<std::string_view>) {
    return type::Slice(type::Char);
  } else {
    NTH_UNREACHABLE("No specified `ic::type::Type` associated with `{}`") <<=
        {t};
  }
}

struct IrContext {
  ParseNode const& Node(ParseNodeIndex index) { return emit.tree[index]; }

  auto ChildIndices(ParseNodeIndex index) {
    return emit.tree.child_indices(index);
  }
  auto Children(ParseNodeIndex index) { return emit.tree.children(index); }

  template <typename T>
  std::optional<T> EvaluateAs(ParseNodeIndex subtree_root_index) {
    T result;
    nth::interval range = emit.tree.subtree_range(subtree_root_index);
    jasmin::ValueStack value_stack;
    emit.Evaluate(range, value_stack, {FromConstant<T>()});
    if constexpr (nth::type<T> == nth::type<std::string_view>) {
      size_t length   = value_stack.pop<size_t>();
      char const* ptr = value_stack.pop<char const*>();
      return std::string_view(ptr, length);
    } else if (IcarusDeserializeValue(
                   std::span(value_stack.begin(), value_stack.end()), result)) {
      return result;
    } else {
      return std::nullopt;
    }
  }

  std::vector<type::QualifiedType>& type_stack() {
    return queue.front().type_stack;
  }

  std::vector<DeclarationInfo>& declaration_stack() {
    return queue.front().declaration_stack;
  }

  std::vector<Token::Kind>& operator_stack() {
    return queue.front().operator_stack;
  }

  void pop_scope() {
    NTH_REQUIRE((v.harden), not queue.empty());
    queue.front().scopes.pop_back();
  }

  void push_scope(Scope::Index index) { queue.front().scopes.push_back(index); }

  Scope::Index current_scope_index() {
    NTH_REQUIRE((v.harden), not queue.front().scopes.empty());
    return queue.front().scopes.back();
  }

  Scope& current_scope() { return emit.scopes[current_scope_index()]; }

  LocalStorage& current_storage() {
    NTH_REQUIRE((v.harden), not queue.front().functions.empty());
    return emit.storage[queue.front().functions.back()];
  }

  struct WorkItem {
    nth::interval<ParseNodeIndex> interval;
    std::vector<type::QualifiedType> type_stack;
    std::vector<Token::Kind> operator_stack;
    std::vector<DeclarationInfo> declaration_stack;
    std::vector<Scope::Index> scopes    = {Scope::Index::Root()};
    std::vector<Scope::Index> functions = {Scope::Index::Root()};
  };

  size_t identifier_repetition_counter = 0;
  std::queue<WorkItem> queue;
  EmitContext& emit;
};

#define IC_XMACRO_PARSE_NODE_CONSTANT(name, t)                                 \
  void HandleParseTreeNode##name(ParseNodeIndex index, IrContext& context,     \
                                 diag::DiagnosticConsumer& diag) {             \
    auto qt = type::QualifiedType::Constant(t);                                \
    context.type_stack().push_back(qt);                                        \
    context.emit.SetQualifiedType(index, qt);                                  \
  }
#include "parse/node.xmacro.h"

bool RequireConstant(type::QualifiedType actual, type::Type expected,
                     diag::DiagnosticConsumer& diag) {
  bool ok = true;
  if (not actual.constant()) {
    ok = false;
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text("The given type expression must be a constant."),
    });
  }
  if (actual.type() != expected) {
    ok = false;
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(InterpolateString<
                   "Expected a value of type `{}` but it was actually `{}`.">(
            expected, actual.type())),
    });
  }
  return ok;
}

void HandleParseTreeNodeDeclaration(ParseNodeIndex index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  DeclarationInfo info = context.declaration_stack().back();
  context.declaration_stack().pop_back();
  type::QualifiedType qt;
  switch (info.kind) {
    case Token::Kind::Colon: {
      type::QualifiedType initializer_qt = context.type_stack().back();
      context.type_stack().pop_back();
      type::QualifiedType type_expr_qt = context.type_stack().back();
      if (not RequireConstant(type_expr_qt, type::Type_, diag)) { return; }
      auto iter          = context.ChildIndices(index).begin();
      auto expr_iter     = iter;
      auto type_iter     = ++iter;
      std::optional type = context.EvaluateAs<type::Type>(*type_iter);
      if (not type) { NTH_UNIMPLEMENTED(); }
      qt = type::QualifiedType::Unqualified(*type);
      context.current_storage().insert(index, qt.type());
    } break;
    case Token::Kind::ColonColon: NTH_UNIMPLEMENTED(); break;
    case Token::Kind::ColonEqual:
      // TODO: Other qualifiers?
      qt = type::QualifiedType::Unqualified(context.type_stack().back().type());
      context.current_storage().insert(index, qt.type());
      goto initializer;
    case Token::Kind::ColonColonEqual:
      // TODO: Other qualifiers?
      qt = type::QualifiedType::Constant(context.type_stack().back().type());
      goto initializer;
    initializer:
      context.current_scope().insert_identifier(
          context.Node(info.index).token.Identifier(),
          {
              .declaration    = info.index,
              .identifier     = index,
              .qualified_type = qt,
          });
      context.type_stack().pop_back();
      break;
    default: NTH_UNREACHABLE();
  }
  context.emit.statement_qualified_type.emplace(index, qt);
}

void HandleParseTreeNodeStatement(ParseNodeIndex index, IrContext& context,
                                  diag::DiagnosticConsumer& diag) {
  context.emit.statement_qualified_type.emplace(index,
                                                context.type_stack().back());
  context.type_stack().pop_back();
}

void HandleParseTreeNodeStatementSequence(ParseNodeIndex index,
                                          IrContext& context,
                                          diag::DiagnosticConsumer& diag) {}

bool HandleParseTreeNodeIdentifier(ParseNodeIndex index, IrContext& context,
                                   diag::DiagnosticConsumer& diag) {
  auto token = context.Node(index).token;
  auto id    = token.Identifier();
  if (context.identifier_repetition_counter > context.queue.size()) {
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Identifier `{}` has no matching declaration.">(
                id)),
        diag::SourceQuote(token),
    });
    context.type_stack().push_back(
        type::QualifiedType::Unqualified(type::Error));
    return true;
  }

  // TODO: Actually want to traverse up the scope until you find it.
  if (auto* decl_info =
          context.emit.scopes.identifier(context.current_scope_index(), id)) {
    auto const& [decl_id_index, decl_index, decl_qt] = *decl_info;
    context.emit.declarator.emplace(index,
                                    std::pair{decl_id_index, decl_index});
    context.type_stack().push_back(decl_qt);
    return true;
  } else {
    auto item     = context.queue.front();
    item.interval = nth::interval(index, item.interval.upper_bound());
    context.queue.push(std::move(item));
    return false;
  }
}

void HandleParseTreeNodeInfixOperator(ParseNodeIndex index, IrContext& context,
                                      diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  context.operator_stack().push_back(node.token.kind());
}

void HandleParseTreeNodeExpressionPrecedenceGroup(
    ParseNodeIndex index, IrContext& context, diag::DiagnosticConsumer& diag) {
  Token::Kind kind = context.operator_stack().back();
  context.operator_stack().pop_back();
  switch (kind) {
    case Token::Kind::Period: break;
    case Token::Kind::MinusGreater: {
      auto node = context.Node(index);
      if (node.child_count != 3) {
        diag.Consume({
            diag::Header(diag::MessageKind::Error),
            diag::Text(
                "The operator `->` is non-associative. Chains of multiple `->` "
                "cannot be used together without parentheses."),
        });
      }
      NTH_REQUIRE(context.type_stack().size() >= 2);
      type::Type return_type = context.type_stack().back().type();
      context.type_stack().pop_back();
      type::Type parameters_type = context.type_stack().back().type();
      auto iter                  = context.Children(index).begin();
      if (parameters_type != type::Type_) {
        auto iter = context.Children(index).begin();
        diag.Consume({
            diag::Header(diag::MessageKind::Error),
            diag::Text(
                InterpolateString<"Function parameters must be types, but "
                                  "you provided a(n) `{}`.">(parameters_type)),
            diag::SourceQuote(iter->token),
        });
      }

      if (return_type != type::Type_) {
        auto iter = context.Children(index).begin();
        diag.Consume({
            diag::Header(diag::MessageKind::Error),
            diag::Text(
                InterpolateString<"Function returns must be types, but you "
                                  "provided a(n) `{}`.">(parameters_type)),
            diag::SourceQuote(iter->token),
        });
      }

      context.type_stack().back() =
          type::QualifiedType(type::Qualifier::Constant(), type::Type_);
    } break;
    default: NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeLet(ParseNodeIndex, IrContext& context,
                            diag::DiagnosticConsumer&) {
  context.declaration_stack().emplace_back();
}

void HandleParseTreeNodeVar(ParseNodeIndex, IrContext& context,
                            diag::DiagnosticConsumer&) {
  context.declaration_stack().emplace_back();
}

void HandleParseTreeNodeColonColonEqual(ParseNodeIndex, IrContext& context,
                                        diag::DiagnosticConsumer&) {
  context.declaration_stack().back().kind = Token::Kind::ColonColonEqual;
}

void HandleParseTreeNodeColonEqual(ParseNodeIndex, IrContext& context,
                                   diag::DiagnosticConsumer&) {
  context.declaration_stack().back().kind = Token::Kind::ColonEqual;
}

void HandleParseTreeNodeColonColon(ParseNodeIndex, IrContext& context,
                                   diag::DiagnosticConsumer&) {
  context.declaration_stack().back().kind = Token::Kind::ColonColon;
}

void HandleParseTreeNodeColon(ParseNodeIndex, IrContext& context,
                              diag::DiagnosticConsumer&) {
  context.declaration_stack().back().kind = Token::Kind::Colon;
}

void HandleParseTreeNodeExpressionGroup(ParseNodeIndex index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer& diag) {
  // Nothing to do here.
}

void HandleParseTreeNodeMemberExpression(ParseNodeIndex index,
                                         IrContext& context,
                                         diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  // IC_PROPAGATE_ERRORS(context, node);
  NTH_REQUIRE(not context.type_stack().empty());
  if (context.type_stack().back().type() == type::Module) {
    if (context.type_stack().back().constant()) {
      auto module_id = context.EvaluateAs<ModuleId>(index - 1);
      NTH_REQUIRE(module_id.has_value());
      auto qt = context.emit.module(*module_id)
                    .Lookup(node.token.Identifier())
                    .qualified_type;
      context.type_stack().back() = qt;
      context.emit.SetQualifiedType(index, qt);
      if (context.type_stack().back().type() == type::Error) {
        diag.Consume({
            diag::Header(diag::MessageKind::Error),
            diag::Text(
                InterpolateString<"No symbol named '{}' in the given module.">(
                    diag.Symbol(context.Node(index).token))),
            diag::SourceQuote(context.Node(index - 1).token),
        });
      }
    } else {
      diag.Consume({
          diag::Header(diag::MessageKind::Error),
          diag::Text("Members may not be accessed from non-constant modules."),
          diag::SourceQuote(context.Node(index - 1).token),
      });
      context.type_stack().back() =
          type::QualifiedType::Unqualified(type::Error);
    }
  } else if (context.type_stack().back().type() == type::Type_) {
    NTH_UNIMPLEMENTED("{} -> {}") <<= {context.type_stack().back(), node.token};
  } else if (context.type_stack().back().type().kind() ==
             type::Type::Kind::Slice) {
    if (context.Node(index).token.Identifier() == Identifier("data")) {
      auto qt = type::QualifiedType::Unqualified(type::BufPtr(
          context.type_stack().back().type().AsSlice().element_type()));
      context.type_stack().back() = qt;
      context.emit.SetQualifiedType(index, qt);
    } else if (context.Node(index).token.Identifier() == Identifier("count")) {
      NTH_UNIMPLEMENTED();
    } else {
      diag.Consume({
          diag::Header(diag::MessageKind::Error),
          diag::Text(InterpolateString<"No member named `{}` in slice type. "
                                       "Only `.data` and `.count` are valid">(
              context.Node(index).token.Identifier())),
          diag::SourceQuote(context.Node(index - 1).token),
      });
      context.type_stack().back() =
          type::QualifiedType::Unqualified(type::Error);
    }
  } else {
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Access operator `.` may only follow a type, "
                              "module, or enum, but you provided: {}.">(
                context.type_stack().back().type())),
        diag::SourceQuote(context.Node(index - 2).token),
    });
    context.type_stack().back().type() = type::Error;
  }
}

void HandleParseTreeNodeCallExpression(ParseNodeIndex index, IrContext& context,
                                       diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  IC_PROPAGATE_ERRORS(context, node);

  auto invocable_type =
      context.type_stack()[context.type_stack().size() - node.child_count];
  if (invocable_type.type().kind() == type::Type::Kind::Function) {
    auto fn_type           = invocable_type.type().AsFunction();
    auto const& parameters = *fn_type.parameters();
    // TODO: Properly implement function call type-checking.
    if (parameters.size() == node.child_count - 1) {
      auto type_iter             = context.type_stack().rbegin();
      auto& argument_width_count = context.emit.rotation_count[index];
      for (size_t i = 0; i < parameters.size(); ++i) {
        argument_width_count += type::JasminSize(type_iter->type());
        ++type_iter;
      }
      auto const& returns = fn_type.returns();
      context.type_stack().resize(context.type_stack().size() -
                                  node.child_count);
      for (type::Type r : returns) {
        context.type_stack().push_back(
            type::QualifiedType(type::Qualifier::Unqualified(), r));
      }

      switch (fn_type.evaluation()) {
        case type::Evaluation::PreferCompileTime: NTH_UNIMPLEMENTED();
        case type::Evaluation::RequireCompileTime: {
          nth::interval range = context.emit.tree.subtree_range(index);
          jasmin::ValueStack value_stack;
          context.emit.Evaluate(range, value_stack, returns);
          // auto module_id = context.EvaluateAs<ModuleId>(index);
          // if (module_id == ModuleId::Invalid()) {
          //   diag.Consume({
          //       diag::Header(diag::MessageKind::Error),
          //       diag::Text(InterpolateString<"No module found named \"{}\"">(
          //           *context.EvaluateAs<std::string_view>(index - 1))),
          //   });
          //   return;
          // }
          // NTH_LOG("{}") <<= {*module_id};
        } break;
        case type::Evaluation::PreferRuntime:
        case type::Evaluation::RequireRuntime: break;
      }
    } else {
      NTH_UNIMPLEMENTED("{} {}") <<= {parameters.size(), node.child_count - 1};
    }
  } else if (invocable_type.type().kind() ==
             type::Type::Kind::GenericFunction) {
    auto& rotation_count = context.emit.rotation_count[index];
    std::vector<ParseNodeIndex> indices;
    for (auto iter = context.ChildIndices(index).begin();
         context.Node(*iter).kind != ParseNode::Kind::InvocationArgumentStart;
         ++iter) {
      indices.push_back(*iter);
    }
    size_t type_stack_index = context.type_stack().size() - indices.size();
    std::reverse(indices.begin(), indices.end());
    jasmin::ValueStack value_stack;
    for (auto index : indices) {
      auto t              = context.type_stack()[type_stack_index].type();
      nth::interval range = context.emit.tree.subtree_range(index);
      context.emit.Evaluate(range, value_stack, {t});
      rotation_count += type::JasminSize(t);
      ++type_stack_index;
    }
    auto g = invocable_type.type().AsGenericFunction();
    jasmin::Execute(*static_cast<IrFunction const*>(g.data()), value_stack);
    auto t = value_stack.pop<type::Type>();
    context.type_stack().push_back(type::QualifiedType::Constant(t));
    NTH_REQUIRE((v.debug), t.kind() == type::Type::Kind::Function);

    if (g.evaluation() == type::Evaluation::PreferCompileTime or
        g.evaluation() == type::Evaluation::RequireCompileTime) {
      jasmin::ValueStack value_stack;
      context.emit.Evaluate(context.emit.tree.subtree_range(index), value_stack,
                            {t});
    }
  } else {
    NTH_UNIMPLEMENTED("node = {} invocable_type = {}") <<=
        {node, invocable_type};
  }
}

void HandleParseTreeNodeDeclaredIdentifier(ParseNodeIndex index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {
  context.declaration_stack().back().index = index;
}

void HandleParseTreeNodeInvocationArgumentStart(
    ParseNodeIndex index, IrContext& context, diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodePointer(ParseNodeIndex index, IrContext& context,
                                diag::DiagnosticConsumer& diag) {
  if (context.type_stack().back().type() != type::Type_) {
    NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeBufferPointer(ParseNodeIndex index, IrContext& context,
                                      diag::DiagnosticConsumer& diag) {
  if (context.type_stack().back().type() != type::Type_) {
    NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeImport(ParseNodeIndex index, IrContext& context,
                               diag::DiagnosticConsumer& diag) {
  std::string_view path = *context.EvaluateAs<std::string_view>(index - 1);
  ModuleId id           = resources.module_map[path];
  nth::interval range   = context.emit.tree.subtree_range(index);
  context.emit.constants.insert_or_assign(
      range, EmitContext::ComputedConstants(index, {id}, {type::Module}));

  if (id == ModuleId::Invalid()) {
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Could not find a module named \"{}\".">(path)),
        diag::SourceQuote(context.Node(index - 1).token),
    });
    context.type_stack().back() = type::QualifiedType::Constant(type::Error);
    return;
  }

  context.type_stack().back() = type::QualifiedType::Constant(type::Module);
}

bool HandleParseTreeNodeScopeStart(ParseNodeIndex index, IrContext& context,
                                   diag::DiagnosticConsumer& diag) {
  auto item         = std::move(context.queue.front());
  auto [start, end] = item.interval;
  // TODO: You used to have this requirement specified:
  //   `NTH_REQUIRE((v.harden), start == index);`
  // It's unclear why this is necessary, and in particular breaks for
  // if-statements.
  auto last_end = end;
  for (auto i : context.emit.tree.child_indices(
           context.Node(index).next_sibling_index)) {
    item.interval = context.emit.tree.subtree_range(i);
    context.queue.push(item);
    // TODO: These are actually ordered (in reverse order), so you can just pick
    // the very first one.
    last_end = std::max(last_end, item.interval.upper_bound());
  }
  if (last_end != end) {
    item.interval = nth::interval(last_end, end);
    context.queue.push(item);
  }

  return false;
}

void HandleParseTreeNodeFunctionStart(ParseNodeIndex index, IrContext& context,
                                      diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodeBeginIfStatementTrueBranch(ParseNodeIndex index,
                                                   IrContext& context,
                                                   diag::DiagnosticConsumer&) {
  context.push_scope(context.Node(index).scope_index);
}

void HandleParseTreeNodeIfStatement(ParseNodeIndex index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  if (context.type_stack().back().type() != type::Bool) { NTH_UNIMPLEMENTED(); }
  context.type_stack().pop_back();
  context.pop_scope();
}

void HandleParseTreeNodeFunctionTypeParameters(ParseNodeIndex index,
                                               IrContext& context,
                                               diag::DiagnosticConsumer& diag) {
  for (size_t i = 0; i < context.emit.Node(index).child_count; ++i) {
    if (context.type_stack().back() !=
        type::QualifiedType::Constant(type::Type_)) {
      NTH_UNIMPLEMENTED("{}") <<= {context.type_stack()};
    }
    context.type_stack().pop_back();
  }
  context.type_stack().push_back(type::QualifiedType::Constant(type::Type_));
}

template <auto F>
constexpr bool Invoke(ParseNodeIndex index, IrContext& context,
                      diag::DiagnosticConsumer& diag) {
  constexpr auto return_type =
      nth::type<std::invoke_result_t<decltype(F), ParseNodeIndex, IrContext&,
                                     diag::DiagnosticConsumer&>>;
  if constexpr (return_type == nth::type<bool>) {
    return F(index, context, diag);
  } else {
    F(index, context, diag);
    return true;
  }
}

void ProcessIrImpl(IrContext& context, diag::DiagnosticConsumer& diag) {
  while (not context.queue.empty()) {
    auto item         = context.queue.front();
    auto [start, end] = item.interval;
    for (auto index = start; index != end; ++index) {
      auto node = context.Node(index);
      switch (node.kind) {
#define IC_XMACRO_PARSE_NODE(kind)                                             \
  case ParseNode::Kind::kind: {                                                \
    NTH_LOG((v.when(debug::type_check)), "Process node {} ({})") <<=           \
        {#kind, index};                                                        \
    bool should_continue =                                                     \
        Invoke<HandleParseTreeNode##kind>(index, context, diag);               \
    if (not should_continue) {                                                 \
      NTH_LOG((v.when(debug::type_check)), "Stopping early after {}") <<=      \
          {index};                                                             \
      goto next_chunk;                                                         \
    }                                                                          \
  } break;
#include "parse/node.xmacro.h"
      }
      context.identifier_repetition_counter = 0;
    }
  next_chunk:
    ++context.identifier_repetition_counter;
    context.queue.pop();
  }
}

}  // namespace

void ProcessIr(EmitContext& emit, diag::DiagnosticConsumer& diag) {
  IrContext context{.emit = emit};
  context.queue.push({.interval = emit.tree.node_range()});
  ProcessIrImpl(context, diag);
}

}  // namespace ic
