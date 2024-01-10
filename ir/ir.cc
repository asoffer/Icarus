#include "ir/ir.h"

#include <optional>
#include <span>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "common/debug.h"
#include "common/module_id.h"
#include "common/resources.h"
#include "common/string.h"
#include "ir/lexical_scope.h"
#include "ir/type_stack.h"
#include "jasmin/core/function.h"
#include "jasmin/core/value.h"
#include "nth/container/stack.h"
#include "nth/debug/debug.h"
#include "parse/node_index.h"
#include "parse/tree.h"
#include "type/cast.h"
#include "type/type.h"

namespace ic {
namespace {

#define IC_PROPAGATE_ERRORS(context, node, count)                              \
  do {                                                                         \
    auto&& c  = (context);                                                     \
    auto&& n  = (node);                                                        \
    auto iter = c.type_stack().rbegin();                                       \
    for (size_t i = 0; i < count; ++i, ++iter) {                               \
      NTH_REQUIRE((v.debug), iter != c.type_stack().rend());                   \
      for (auto qt : *iter) {                                                  \
        if (qt.type() == type::Error) {                                        \
          for (size_t j = 0; j < count; ++j) { c.type_stack().pop(); }         \
          c.type_stack().push(                                                 \
              {type::QualifiedType::Unqualified(type::Error)});                \
          return;                                                              \
        }                                                                      \
      }                                                                        \
    }                                                                          \
  } while (false);

type::QualifiedType QualifiedBy(type::Type t, DeclarationInfo info) {
  type::Qualifier q = type::Qualifier::Unqualified();
  if (info.kind.constant()) { q |= type::Qualifier::Constant(); }
  if (info.kind.addressable()) { q |= type::Qualifier::Addressable(); }
  return type::QualifiedType(q, t);
}

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
std::optional<type::QualifiedType> FindInfixOperator(
    Token::Kind kind, std::span<type::QualifiedType const> argument_types) {
  if (argument_types.size() != 2) { return std::nullopt; }
  if (type::ImplicitCast(AnyValue::JustType(argument_types[1].type()), argument_types[0].type())) {
    return type::QualifiedType::Unqualified(argument_types[0].type());
  }
  return std::nullopt;
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
    nth::stack<jasmin::Value> value_stack;
    emit.Evaluate(range, value_stack, {FromConstant<T>()});
    if constexpr (nth::type<T> == nth::type<std::string_view>) {
      size_t length   = value_stack.top().as<size_t>();
      value_stack.pop();
      char const* ptr = value_stack.top().as<char const*>();
      value_stack.pop();
      return std::string_view(ptr, length);
    } else if (IcarusDeserializeValue(value_stack.top_span(value_stack.size()),
                                      result)) {
      return result;
    } else {
      return std::nullopt;
    }
  }

  TypeStack& type_stack() { return queue.front().type_stack_; }

  void PopTypeStack(size_t num_to_pop) {
    auto& ts = type_stack();
    for (size_t i = 0; i < num_to_pop; ++i) { ts.pop(); }
  }
  void MakeError(size_t num_to_pop) {
    PopTypeStack(num_to_pop);
    type_stack().push({type::QualifiedType::Unqualified(type::Error)});
  }

  nth::stack<DeclarationInfo>& declaration_stack() {
    return queue.front().declaration_stack;
  }

  std::vector<Token::Kind>& operator_stack() {
    return queue.front().operator_stack;
  }

  void pop_lexical_scope() {
    NTH_REQUIRE((v.harden), not queue.empty());
    queue.front().lexical_scopes.pop_back();
  }

  void push_lexical_scope(LexicalScope::Index index) {
    queue.front().lexical_scopes.push_back(index);
  }

  LexicalScope::Index current_lexical_scope_index() {
    NTH_REQUIRE((v.harden), not queue.front().lexical_scopes.empty());
    return queue.front().lexical_scopes.back();
  }

  LexicalScope& current_lexical_scope() {
    return emit.lexical_scopes[current_lexical_scope_index()];
  }

  LocalStorage& current_storage() {
    NTH_REQUIRE((v.harden), not queue.front().functions.empty());
    return emit.storage[queue.front().functions.back()];
  }

  struct WorkItem {
    WorkItem(nth::interval<ParseNodeIndex> interval) : interval(interval) {}

    nth::interval<ParseNodeIndex> interval;
    std::vector<Token::Kind> operator_stack;
    nth::stack<DeclarationInfo> declaration_stack;
    std::vector<LexicalScope::Index> lexical_scopes;
    std::vector<LexicalScope::Index> functions;

   private:
    friend IrContext;
    TypeStack type_stack_;
  };

  size_t identifier_repetition_counter = 0;
  std::queue<WorkItem> queue;
  EmitContext& emit;
};

#define IC_XMACRO_PARSE_NODE_CONSTANT(name, t)                                 \
  void HandleParseTreeNode##name(ParseNodeIndex index, IrContext& context,     \
                                 diag::DiagnosticConsumer& diag) {             \
    auto qt = type::QualifiedType::Constant(t);                                \
    context.type_stack().push({qt});                                           \
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

void HandleParseTreeNodeModule(ParseNodeIndex index, IrContext& context,
                               diag::DiagnosticConsumer& diag) {
  context.queue.front().functions.pop_back();
  context.pop_lexical_scope();
}

void HandleParseTreeNodeModuleStart(ParseNodeIndex index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  context.queue.front().functions.push_back(LexicalScope::Index::Root());
  context.push_lexical_scope(LexicalScope::Index::Root());
}

#define IC_XMACRO_PARSE_NODE_PREFIX_UNARY(node, token, precedence)             \
  void HandleParseTreeNode##node##Start(ParseNodeIndex index,                  \
                                        IrContext& context,                    \
                                        diag::DiagnosticConsumer& diag) {}
#include "parse/node.xmacro.h"

// Returns true if the error was diagnosed.
bool TryDiagnoseUnexpanded(TypeStack& type_stack,
                           diag::DiagnosticConsumer& diag, Token token) {
  if (type_stack.top().size() == 1) { return false; }
  diag.Consume({
      diag::Header(diag::MessageKind::Error),
      diag::Text("Expression represents an unexpanded pack of values and "
                 "cannot be used in this context. Expand the arguments first."),
      diag::SourceQuote(token),
  });
  type_stack.pop();
  type_stack.push({type::QualifiedType::Unqualified(type::Error)});
  return true;
}

void HandleParseTreeNodeDeref(ParseNodeIndex index, IrContext& context,
                             diag::DiagnosticConsumer& diag) {
  if (TryDiagnoseUnexpanded(context.type_stack(), diag,
                            context.Node(index - 1).token)) {
    return;
  }
  auto qt = context.type_stack().top()[0];
  context.emit.SetQualifiedType(index - 1, qt);
  context.type_stack().pop();
  switch (qt.type().kind()) {
    case type::Type::Kind::Pointer:
      context.type_stack().push(
          {type::QualifiedType::Unqualified(qt.type().AsPointer().pointee())});
      break;
    case type::Type::Kind::BufferPointer:
      context.type_stack().push({type::QualifiedType::Unqualified(
          qt.type().AsBufferPointer().pointee())});
      break;
    default: NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeScopeBlockStart(ParseNodeIndex index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer& diag) {
  LexicalScope::Index scope_index = context.Node(index).scope_index;
  context.push_lexical_scope(scope_index);
}

void HandleParseTreeNodeScopeBlock(ParseNodeIndex index, IrContext& context,
                                   diag::DiagnosticConsumer& diag) {
  context.pop_lexical_scope();
}

void HandleParseTreeNodeScopeBodyStart(ParseNodeIndex index, IrContext& context,
                                       diag::DiagnosticConsumer& diag) {
  context.type_stack().pop();
}

void HandleParseTreeNodeScope(ParseNodeIndex index, IrContext& context,
                              diag::DiagnosticConsumer& diag) {
  context.type_stack().push({});
}

void HandleParseTreeNodeScopeLiteral(ParseNodeIndex index, IrContext& context,
                                     diag::DiagnosticConsumer& diag) {
  context.type_stack().push({type::QualifiedType::Constant(type::Scope)});
}

Iteration HandleParseTreeNodeScopeLiteralStart(ParseNodeIndex index,
                                               IrContext& context,
                                               diag::DiagnosticConsumer& diag) {
  context.current_lexical_scope().insert_identifier(
      context.Node(index + 1).token.Identifier(),
      {
          .declaration    = index + 1,
          .identifier     = index + 1,
          .qualified_type = type::QualifiedType::Constant(type::Bool),  // TODO
      });
  return Iteration::SkipTo(index + 2);
}

void HandleParseTreeNodeMinus(ParseNodeIndex index, IrContext& context,
                              diag::DiagnosticConsumer& diag) {
  IC_PROPAGATE_ERRORS(context, context.Node(index), 1);
  // TODO: Type-check.
  context.emit.SetQualifiedType(index, context.emit.QualifiedTypeOf(index - 1));
  return;
}

void HandleParseTreeNodeAddress(ParseNodeIndex index, IrContext& context,
                                diag::DiagnosticConsumer& diag) {
  if (TryDiagnoseUnexpanded(context.type_stack(), diag,
                            context.Node(index - 1).token)) {
    return;
  }
  auto qt = context.type_stack().top()[0];
  context.type_stack().pop();
  if (qt.addressable()) {
    context.type_stack().push(
        {type::QualifiedType::Unqualified(type::Ptr(qt.type()))});
  } else {
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text("Expression is not addressable"),
        diag::SourceQuote(context.Node(index - 1).token),
    });
    context.type_stack().push({type::QualifiedType::Unqualified(type::Error)});
  }
}

void HandleParseTreeNodeDeclaration(ParseNodeIndex index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  DeclarationInfo info = context.declaration_stack().top();
  context.declaration_stack().pop();
  if (not info.kind.has_initializer()) {
    NTH_REQUIRE((v.debug), not info.kind.inferred_type());
    context.type_stack().pop();
    std::optional type = context.EvaluateAs<type::Type>(index - 1);
    if (not type) { NTH_UNIMPLEMENTED(); }

    auto qt = QualifiedBy(*type, info);
    context.current_lexical_scope().insert_identifier(
        context.Node(info.index).token.Identifier(),
        {
            .declaration    = info.index,
            .identifier     = index,
            .qualified_type = qt,
        });

    context.current_storage().insert(index, *type);
    context.emit.SetQualifiedType(index, qt);
  } else if (info.kind.inferred_type()) {
    IC_PROPAGATE_ERRORS(context, context.Node(index), 1);
    type::QualifiedType qt =
        QualifiedBy(context.type_stack().top()[0].type(), info);
    if (not info.kind.constant()) {
      context.current_storage().insert(index, qt.type());
    }
    context.emit.SetQualifiedType(index, qt);

    context.current_lexical_scope().insert_identifier(
        context.Node(info.index).token.Identifier(),
        {
            .declaration    = info.index,
            .identifier     = index,
            .qualified_type = qt,
        });
    context.type_stack().pop();
  } else {
    auto types_iter      = context.type_stack().rbegin();
    std::span init_types = *types_iter;
    std::span type_types = *++types_iter;

    if (init_types.size() != 1) {
      NTH_UNIMPLEMENTED("Log an error");
      context.MakeError(2);
    }

    if (type_types.size() != 1) {
      NTH_UNIMPLEMENTED("Log an error");
      context.MakeError(2);
    } else if (not RequireConstant(type_types[0], type::Type_, diag)) {
      context.MakeError(2);
      return;
    }

    type::QualifiedType init_qt = init_types[0];
    context.type_stack().pop();
    context.type_stack().pop();

    auto iter          = context.ChildIndices(index).begin();
    auto expr_iter     = iter;
    auto type_iter     = ++iter;
    std::optional type = context.EvaluateAs<type::Type>(*type_iter);
    if (not type) { NTH_UNIMPLEMENTED(); }
    if (not type::ImplicitCast(AnyValue::JustType(init_qt.type()), *type)) {
      auto token = context.Node(index).token;
    diag.Consume({
          diag::Header(diag::MessageKind::Error),
          diag::Text(InterpolateString<"Initializing expression does not match "
                                       "declared type ({} vs {}).">(
              init_qt.type(), *type)),
          diag::SourceQuote(token),
      });
      context.type_stack().push(
          {type::QualifiedType::Unqualified(type::Error)});
    }
    type::QualifiedType qt = type::QualifiedType::Unqualified(*type);
    context.current_storage().insert(index, qt.type());
    context.current_lexical_scope().insert_identifier(
        context.Node(info.index).token.Identifier(),
        {
            .declaration    = info.index,
            .identifier     = index,
            .qualified_type = qt,
        });
    context.emit.SetQualifiedType(index, qt);
  }
}

void HandleParseTreeNodeStatement(ParseNodeIndex index, IrContext& context,
                                  diag::DiagnosticConsumer& diag) {
  switch (context.Node(context.emit.tree.first_descendant_index(index))
              .statement_kind) {
    case ParseNode::StatementKind::Expression: {
      IC_PROPAGATE_ERRORS(context, context.Node(index), 1);
      std::span qts = context.type_stack().top();
      size_t size   = 0;
      type::ByteWidth bytes(0);
      for (auto qt : qts) {
        auto contour = type::Contour(qt.type());
        bytes.align_forward_to(contour.alignment());
        bytes += contour.byte_width();
        size += type::JasminSize(qt.type());
      }

      context.emit.statement_expression_info.emplace(
          index, std::make_pair(bytes, size));
      context.type_stack().pop();
    } break;
    default: break;
  }
}

void HandleParseTreeNodeStatementSequence(ParseNodeIndex index,
                                          IrContext& context,
                                          diag::DiagnosticConsumer& diag) {}

Iteration HandleParseTreeNodeIdentifier(ParseNodeIndex index,
                                        IrContext& context,
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
    context.type_stack().push({type::QualifiedType::Unqualified(type::Error)});
    return Iteration::Continue;
  }

  // TODO: Actually want to traverse up the scope until you find it.
  if (auto* decl_info = context.emit.lexical_scopes.identifier(
          context.current_lexical_scope_index(), id)) {
    auto const& [decl_id_index, decl_index, decl_qt] = *decl_info;
    context.emit.declarator.emplace(index,
                                    std::pair{decl_id_index, decl_index});
    context.emit.SetQualifiedType(index, decl_qt);
    context.type_stack().push({decl_qt});
    return Iteration::Continue;
  } else {
    auto item     = context.queue.front();
    item.interval = nth::interval(index, item.interval.upper_bound());
    context.queue.push(std::move(item));
    return Iteration::PauseRetry;
  }
}

void HandleParseTreeNodeInfixOperator(ParseNodeIndex index, IrContext& context,
                                      diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  context.operator_stack().push_back(node.token.kind());
}

void HandleParseTreeNodeExpressionPrecedenceGroup(
    ParseNodeIndex index, IrContext& context, diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  IC_PROPAGATE_ERRORS(context, node, (1 + node.child_count) / 2);
  Token::Kind kind = context.operator_stack().back();
  context.operator_stack().pop_back();
  switch (kind) {
    case Token::Kind::Period: break;
    case Token::Kind::MinusGreater: {
      if (node.child_count != 3) {
        diag.Consume({
            diag::Header(diag::MessageKind::Error),
            diag::Text(
                "The operator `->` is non-associative. Chains of multiple `->` "
                "cannot be used together without parentheses."),
        });
      }
      NTH_REQUIRE(context.type_stack().group_count() >= 2);
      std::span return_types = context.type_stack().top();
      std::optional<type::Type> return_type;
      if (not return_types.empty()) { return_type = return_types[0].type(); }
      context.type_stack().pop();
      type::Type parameters_type = context.type_stack().top()[0].type();
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

      if (return_type and return_type != type::Type_) {
        auto iter = context.Children(index).begin();
        diag.Consume({
            diag::Header(diag::MessageKind::Error),
            diag::Text(
                InterpolateString<"Function returns must be types, but you "
                                  "provided a(n) `{}`.">(parameters_type)),
            diag::SourceQuote(iter->token),
        });
      }

      context.type_stack().pop();
      context.type_stack().push({type::QualifiedType::Constant(type::Type_)});
    } break;
    case Token::Kind::Plus:
    case Token::Kind::Minus:
    case Token::Kind::Percent:
    case Token::Kind::Star: {
      NTH_REQUIRE(context.type_stack().group_count() >= node.child_count / 2);
      NTH_REQUIRE(node.child_count >= 3);
      auto iter = context.type_stack().rbegin();
      std::vector<type::QualifiedType> types;
      for (size_t i = 0; i <= node.child_count / 2; ++i, ++iter) {
        if ((*iter).size() != 1) { NTH_UNIMPLEMENTED(); }
        types.push_back((*iter)[0]);
      }
      std::reverse(types.begin(), types.end());
      auto start = types.begin();
      type::QualifiedType current;
      for (auto end = start + 1; end != types.end(); ++end) {
        if (auto return_type =
                FindInfixOperator(kind, std::span(start, end + 1))) {
          current = *return_type;
        } else {
          if (start + 1 == end) { NTH_UNIMPLEMENTED(); }
          start  = end;
          *start = std::exchange(current,
                                 type::QualifiedType::Unqualified(type::Error));
        }
      }

      if (current.type() == type::Error) { NTH_UNIMPLEMENTED(); }
      context.PopTypeStack(1 + node.child_count / 2);
      context.type_stack().push({current});
    } break;
    case Token::Kind::As: {
      if (context.type_stack().top()[0] !=
          type::QualifiedType::Constant(type::Type_)) {
        NTH_UNIMPLEMENTED();
      }
      context.PopTypeStack(1 + node.child_count / 2);
      std::optional t = context.EvaluateAs<type::Type>(index - 1);
      if (not t) { NTH_UNIMPLEMENTED(); }
      context.type_stack().push({type::QualifiedType::Constant(*t)});
    } break;
    case Token::Kind::Less:
    case Token::Kind::Greater:
    case Token::Kind::LessEqual:
    case Token::Kind::GreaterEqual:
    case Token::Kind::EqualEqual:
    case Token::Kind::NotEqual: {
      NTH_REQUIRE(context.type_stack().group_count() >= node.child_count / 2);
      auto iter = context.type_stack().rbegin();
      std::vector<type::QualifiedType> types;
      for (size_t i = 0; i <= node.child_count / 2; ++i, ++iter) {
        if ((*iter).size() != 1) { NTH_UNIMPLEMENTED(); }
        types.push_back((*iter)[0]);
      }
      for (size_t i = 0; i + 1 < types.size(); ++i) {
        if (not type::ImplicitCast(AnyValue::JustType(types[i].type()),
                                   types[i + 1].type()) and
            not type::ImplicitCast(AnyValue::JustType(types[i + 1].type()),
                                   types[i].type())) {
          NTH_UNIMPLEMENTED();
        }
      }
      context.PopTypeStack(1 + node.child_count / 2);
      context.type_stack().push({type::QualifiedType::Constant(type::Bool)});
    } break;
    default: NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeDeclarationStart(ParseNodeIndex index,
                                         IrContext& context,
                                         diag::DiagnosticConsumer&) {
  context.declaration_stack().push(context.Node(index).declaration_info);
}

void HandleParseTreeNodeMemberExpression(ParseNodeIndex index,
                                         IrContext& context,
                                         diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  IC_PROPAGATE_ERRORS(context, node, 1);
  NTH_REQUIRE(not context.type_stack().empty());
  context.emit.SetQualifiedType(index - 1, context.type_stack().top()[0]);
  if (context.type_stack().top()[0].type() == type::Module) {
    if (context.type_stack().top()[0].constant()) {
      auto module_id = context.EvaluateAs<ModuleId>(index - 1);
      NTH_REQUIRE(module_id.has_value());
      auto qt = context.emit.module(*module_id)
                    .Lookup(node.token.Identifier())
                    .qualified_type;
      context.type_stack().pop();
      context.type_stack().push({qt});
      context.emit.SetQualifiedType(index, qt);
      if (context.type_stack().top()[0].type() == type::Error) {
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
      context.type_stack().pop();
      context.type_stack().push(
          {type::QualifiedType::Unqualified(type::Error)});
    }
  } else if (context.type_stack().top()[0].type() == type::Type_) {
    NTH_UNIMPLEMENTED("{} -> {}") <<= {context.type_stack().top(), node.token};
  } else if (context.type_stack().top()[0].type().kind() ==
             type::Type::Kind::Slice) {
    if (context.Node(index).token.Identifier() == Identifier("data")) {
      auto qt = type::QualifiedType::Unqualified(type::BufPtr(
          context.type_stack().top()[0].type().AsSlice().element_type()));
      context.type_stack().pop();
      context.type_stack().push({qt});
      context.emit.SetQualifiedType(index, qt);
    } else if (context.Node(index).token.Identifier() == Identifier("count")) {
      auto qt = type::QualifiedType::Unqualified(type::U64);
      context.type_stack().pop();
      context.type_stack().push({qt});
      context.emit.SetQualifiedType(index, qt);
    } else {
      diag.Consume({
          diag::Header(diag::MessageKind::Error),
          diag::Text(InterpolateString<"No member named `{}` in slice type. "
                                       "Only `.data` and `.count` are valid">(
              context.Node(index).token.Identifier())),
          diag::SourceQuote(context.Node(index - 1).token),
      });
      context.type_stack().pop();
      context.type_stack().push(
          {type::QualifiedType::Unqualified(type::Error)});
    }
  } else {
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Access operator `.` may only follow a type, "
                              "module, or enum, but you provided: {}.">(
                context.type_stack().top()[0].type())),
        diag::SourceQuote(context.Node(index - 2).token),
    });
    context.type_stack().pop();
    context.type_stack().push({type::QualifiedType::Unqualified(type::Error)});
  }
}

void HandleParseTreeNodeIndexExpression(ParseNodeIndex index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer& diag) {
  std::vector<type::QualifiedType> index_argument_qts;
  auto iter = context.ChildIndices(index).begin();
  for (; context.Node(*iter).kind != ParseNode::Kind::IndexArgumentStart;
       ++iter) {
    if (context.type_stack().top().size() != 1) { NTH_UNIMPLEMENTED(); }
    index_argument_qts.push_back(context.type_stack().top()[0]);
    context.type_stack().pop();
  }

  for (auto qt :index_argument_qts ) {
    if (qt.type() == type::Error) {
      context.type_stack().pop();
      context.type_stack().push(
          {type::QualifiedType::Unqualified(type::Error)});
      return;
    }
  }
  std::reverse(index_argument_qts.begin(), index_argument_qts.end());

  ++iter;
  if (TryDiagnoseUnexpanded(context.type_stack(), diag,
                            context.Node(*iter).token)) {
    return;
  }
  auto qt = context.type_stack().top()[0];
  context.emit.SetQualifiedType(*iter, qt);
  context.type_stack().pop();
  if (qt.type() == type::Error) {
    context.type_stack().push({type::QualifiedType::Unqualified(type::Error)});
    return;
  }

  switch (qt.type().kind()) {
    case type::Type::Kind::Slice: {
      if (index_argument_qts.size() == 1) {
        if (index_argument_qts[0].type().kind() ==
                type::Type::Kind::Primitive and
            type::Integral(index_argument_qts[0].type().AsPrimitive())) {
          context.type_stack().push({type::QualifiedType(
              qt.qualifier(), qt.type().AsSlice().element_type())});
        } else {
          NTH_UNIMPLEMENTED();
        }
      } else {
        NTH_UNIMPLEMENTED();
      }
    } break;
    case type::Type::Kind::BufferPointer: {
      if (index_argument_qts.size() == 1) {
        if (index_argument_qts[0].type().kind() ==
                type::Type::Kind::Primitive and
            type::Integral(index_argument_qts[0].type().AsPrimitive())) {
          context.type_stack().push({type::QualifiedType(
              qt.qualifier(), qt.type().AsBufferPointer().pointee())});
        } else {
          NTH_UNIMPLEMENTED();
        }
      } else {
        NTH_UNIMPLEMENTED();
      }
    } break;
    case type::Type::Kind::Primitive: {
      if (qt.type() != type::Type_) { NTH_UNIMPLEMENTED("{}") <<= {qt}; }
      // TODO: If the pattern isn't constant that should mean the entire thing
      // isn't a constant.
      context.type_stack().push({qt});
    } break;
      default: NTH_UNIMPLEMENTED("{}") <<= {qt};
  }
}

struct InvocationSuccess {};
struct ParameterArgumentCountMismatch {
  size_t parameters;
  size_t arguments;
};
struct InvalidBinding{
  size_t index;
  type::Type parameter;
  type::Type argument;
};
using InvocationResult =
    std::variant<InvocationSuccess, ParameterArgumentCountMismatch, InvalidBinding>;

struct CallArguments {
  struct Argument {
    ParseNodeIndex index;
    type::QualifiedType qualified_type;

    type::Type type() const { return qualified_type.type(); }
  };

  InvocationResult Invoke(type::FunctionType fn_type) {
    auto const& parameters = *fn_type.parameters();
    // TODO: Properly implement function call type-checking.
    if (parameters.size() != arguments.size()) {
      return ParameterArgumentCountMismatch{.parameters = parameters.size(),
                                            .arguments  = arguments.size()};
    }

    for (size_t i = 0; i < arguments.size(); ++i) {
      if (not type::ImplicitCast(AnyValue::JustType(arguments[i].type()),
                                 parameters[i].type)) {
        return InvalidBinding{
            .index     = i,
            .parameter = parameters[i].type,
            .argument  = arguments[i].type(),
        };
      }
    }

    return InvocationSuccess{};
  }

  jasmin::InstructionSpecification MakeInstructionSpecification() const {
    jasmin::InstructionSpecification spec{.parameters = 1, .returns = 0};
    auto fn_type = callee.type().AsFunction();
    auto iter = (*fn_type.parameters()).begin();
    for (size_t i = 0; i < std::distance(postfix_start, arguments.end()); ++i) {
      spec.parameters += type::JasminSize(iter->type);
      ++iter;
    }

    for (type::Type r : fn_type.returns()) {
      spec.returns += type::JasminSize(r);
    }
    return spec;
  }

  Argument callee;
  std::vector<Argument> arguments;
  std::vector<Argument>::const_iterator postfix_start;
};

bool PopulateCall(ParseNodeIndex index, IrContext& context,
                  CallArguments& call) {
  auto& type_stack  = context.type_stack();
  auto nodes        = context.ChildIndices(index);
  int postfix_count = -1;
  bool postfix      = true;
  for (auto node_iter = nodes.begin(); node_iter != nodes.end(); ++node_iter) {
    auto const& child_node = context.Node(*node_iter);
    if (child_node.kind == ParseNode::Kind::PrefixInvocationArgumentEnd) {
      NTH_REQUIRE((v.debug), not call.arguments.empty());
      postfix     = false;
      call.callee = call.arguments.back();
      call.arguments.pop_back();
      continue;
    }

    if (child_node.kind == ParseNode::Kind::InvocationArgumentStart) {
      continue;
    }

    auto qts = type_stack.top();
    if (qts.size() != 1) { NTH_UNIMPLEMENTED("Unexpanded"); }
    if (qts[0].type() == type::Error) {
      for (; node_iter != nodes.end(); ++node_iter) { type_stack.pop(); }
      type_stack.push({type::QualifiedType::Unqualified(type::Error)});
      return false;
    }
    call.arguments.push_back({.index = *node_iter, .qualified_type = qts[0]});
    type_stack.pop();
    if (postfix) { ++postfix_count; }
  }

  if (postfix) {
    call.callee = call.arguments.back();
    call.arguments.pop_back();
  }
  std::reverse(call.arguments.begin(), call.arguments.end());
  call.postfix_start = call.arguments.end() - postfix_count;
  return true;
}

void HandleParseTreeNodeCallExpression(ParseNodeIndex index, IrContext& context,
                                       diag::DiagnosticConsumer& diag) {
  CallArguments call;
  if (not PopulateCall(index, context, call)) { return; }
  auto node = context.Node(index);

  if (call.callee.type().kind() == type::Type::Kind::Function) {
    auto fn_type           = call.callee.type().AsFunction();
    InvocationResult result = call.Invoke(fn_type);
    bool success = std::visit(
        [&](auto const& r) {
          constexpr auto t = nth::type<decltype(r)>.decayed();
          if constexpr (t == nth::type<InvocationSuccess>) {
            return true;
          } else if constexpr (t == nth::type<ParameterArgumentCountMismatch>) {
            diag.Consume({
                diag::Header(diag::MessageKind::Error),
                diag::Text(InterpolateString<
                           "Incorrect number of arguments passed to function: "
                                      "Expected {}, but {} were provided.">(r.parameters,
                                                                 r.arguments)),
                diag::SourceQuote(context.Node(index).token),
            });
          } else if constexpr (t == nth::type<InvalidBinding>) {
            diag.Consume({
                diag::Header(diag::MessageKind::Error),
                diag::Text(InterpolateString<
                           "Argument at position {} cannot be passed to "
                                      "function. Expected a {} but argument has type {}">(
                    r.index, r.parameter, r.argument)),
                diag::SourceQuote(context.Node(index).token),
            });
          }
          return false;
        },
        result);
    if (not success) {
      context.type_stack().push(
          {type::QualifiedType::Unqualified(type::Error)});
      return;
    }

    auto [iter, inserted]  = context.emit.instruction_spec.try_emplace(
         index, call.MakeInstructionSpecification());
    NTH_REQUIRE((v.harden), inserted);
    auto const& returns = fn_type.returns();
    std::vector<type::QualifiedType> return_qts;
    for (type::Type r : returns) {
      return_qts.push_back(type::QualifiedType::Unqualified(r));
    }
    context.type_stack().push(return_qts);

    switch (fn_type.evaluation()) {
      case type::Evaluation::PreferCompileTime: NTH_UNIMPLEMENTED();
      case type::Evaluation::RequireCompileTime: {
        nth::interval range = context.emit.tree.subtree_range(index);
        nth::stack<jasmin::Value> value_stack;
        context.emit.Evaluate(range, value_stack, returns);
        auto module_id = context.EvaluateAs<ModuleId>(index);
        if (module_id == ModuleId::Invalid()) {
          diag.Consume({
              diag::Header(diag::MessageKind::Error),
              diag::Text(InterpolateString<"No module found named \"{}\"">(
                  *context.EvaluateAs<std::string_view>(index - 1))),
          });
          return;
        }
      } break;
      case type::Evaluation::PreferRuntime:
      case type::Evaluation::RequireRuntime: break;
    }
  } else if (call.callee.type().kind() == type::Type::Kind::DependentFunction) {
    auto& spec = context.emit.instruction_spec[index];
    ++spec.parameters;
    std::vector<AnyValue> arguments;

    for (auto [index, qt] : call.arguments) {
      nth::stack<jasmin::Value> value_stack;
      nth::interval range = context.emit.tree.subtree_range(index);
      if (qt.constant()) {
        context.emit.Evaluate(range, value_stack, {qt.type()});
        std::span values = value_stack.top_span(value_stack.size());
        arguments.emplace_back(qt.type(),
                               std::vector(values.begin(), values.end()));
      } else {
        arguments.emplace_back(qt.type(), std::vector<jasmin::Value>{});
      }
    }

    for (auto iter = call.postfix_start; iter != call.arguments.end(); ++iter) {
      spec.parameters += type::JasminSize(iter->type());
    }

    auto dep = call.callee.type().AsDependentFunction();
    std::optional t = dep(arguments);
    if (not t) {
      diag.Consume({
          diag::Header(diag::MessageKind::Error),
          diag::Text(
              "Unable to call dependent function with the given arguments."),
      });
      context.type_stack().push(
          {type::QualifiedType::Unqualified(type::Error)});
      return;
    }
    context.type_stack().push({type::QualifiedType::Constant(*t)});

    nth::stack<jasmin::Value> value_stack;
    context.emit.Evaluate(context.emit.tree.subtree_range(index), value_stack,
                          {*t});
  } else {
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(InterpolateString<"Objects of type {} are not invocable.">(
            call.callee.type())),
    });
    context.type_stack().push({type::QualifiedType::Unqualified(type::Error)});
    return;

  }
}

void HandleParseTreeNodeEmptyParenthesis(ParseNodeIndex index,
                                         IrContext& context,
                                         diag::DiagnosticConsumer& diag) {
  context.type_stack().push({});
}

void HandleParseTreeNodeDeclaredIdentifier(ParseNodeIndex index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {
  context.declaration_stack().top().index = index;
}

void HandleParseTreeNodeIndexArgumentStart(ParseNodeIndex index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodeNamedArgument(ParseNodeIndex index, IrContext& context,
                                      diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodeNamedArgumentStart(ParseNodeIndex index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodePrefixInvocationArgumentEnd(
    ParseNodeIndex index, IrContext& context, diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodeInvocationArgumentStart(
    ParseNodeIndex index, IrContext& context, diag::DiagnosticConsumer& diag) {}

void TryDiagnoseUnaryTypeConstructorError(TypeStack& type_stack,
                                          diag::DiagnosticConsumer& diag,
                                          std::string_view type_constructor,
                                          Token token) {
  if (type_stack.top()[0].type() == type::Type_) { return; }
  diag.Consume({
      diag::Header(diag::MessageKind::Error),
      diag::Text(
          InterpolateString<"Attempting to apply the {} type constructor "
                            "`\\` to a non-type (specifically, a(n) {}).">(
              type_constructor, type_stack.top()[0].type())),
      diag::SourceQuote(token),
  });
auto q  = type_stack.top()[0].qualifier();
  type_stack.pop();
  type_stack.push({type::QualifiedType(q, type::Error)});
}

void HandleParseTreeNodePointer(ParseNodeIndex index, IrContext& context,
                                diag::DiagnosticConsumer& diag) {
  IC_PROPAGATE_ERRORS(context, context.Node(index), 1);
  TryDiagnoseUnaryTypeConstructorError(context.type_stack(), diag, "pointer",
                                       context.Node(index - 1).token);
}

void HandleParseTreeNodeBufferPointer(ParseNodeIndex index, IrContext& context,
                                      diag::DiagnosticConsumer& diag) {
  IC_PROPAGATE_ERRORS(context, context.Node(index), 1);
  TryDiagnoseUnaryTypeConstructorError(context.type_stack(), diag,
                                       "buffer-pointer",
                                       context.Node(index - 1).token);
}

void HandleParseTreeNodeSlice(ParseNodeIndex index, IrContext& context,
                              diag::DiagnosticConsumer& diag) {
  IC_PROPAGATE_ERRORS(context, context.Node(index), 1);
  TryDiagnoseUnaryTypeConstructorError(context.type_stack(), diag, "slice",
                                       context.Node(index - 1).token);
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
    context.type_stack().pop();
    context.type_stack().push({type::QualifiedType::Constant(type::Error)});
    return;
  }

  context.type_stack().pop();
  context.type_stack().push({type::QualifiedType::Constant(type::Module)});
}

Iteration HandleParseTreeNodeScopeStart(ParseNodeIndex index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer& diag) {
  auto item           = std::move(context.queue.front());
  auto [start, end]   = item.interval;
  auto stmt_seq_index = context.Node(index).corresponding_statement_sequence;
  auto child_indices  = context.emit.tree.child_indices(stmt_seq_index);
  for (auto i : child_indices) {
    // TODO: We always just want to ignore the last one. There's probably a
    // better way to do this.
    if (i == index) { continue; }
    item.interval = context.emit.tree.subtree_range(i);
    context.queue.push(item);
  }
  item.interval = nth::interval(stmt_seq_index, end);
  context.queue.push(item);

  return Iteration::PauseMoveOn;
}

void HandleParseTreeNodeFunctionLiteralStart(ParseNodeIndex index,
                                             IrContext& context,
                                             diag::DiagnosticConsumer& diag) {
  LexicalScope::Index scope_index = context.Node(index).scope_index;
  context.queue.front().functions.push_back(scope_index);
  context.push_lexical_scope(scope_index);
}

void HandleParseTreeNodeFunctionLiteralSignature(
    ParseNodeIndex index, IrContext& context, diag::DiagnosticConsumer& diag) {
  ParseNodeIndex start = context.emit.tree.first_descendant_index(index) - 1;
  NTH_REQUIRE((v.debug), context.Node(start).kind ==
                             ParseNode::Kind::FunctionLiteralStart);
  std::vector<type::Type> return_types;
  std::vector<type::ParametersType::Parameter> parameters;
  auto indices = context.ChildIndices(index);

  auto iter = indices.begin();
  switch (context.Node(*iter).kind) {
    case ParseNode::Kind::NoReturns: break;
    default:
      if (std::optional t = context.EvaluateAs<type::Type>(*iter)) {
        return_types.push_back(*t);
      } else {
        NTH_UNIMPLEMENTED();
      }
  }
  ++iter;

  for (; iter != indices.end(); ++iter) {
    // Get DeclaredIdentifier from declaration.
    // TODO: There may be many!
    auto decl_iter = context.Children(*iter).begin();
    while (decl_iter->kind != ParseNode::Kind::DeclaredIdentifier) {
      ++decl_iter;
    }

    if (auto* decl_info = context.current_lexical_scope().identifier(
            decl_iter->token.Identifier())) {
      parameters.push_back({.type = decl_info->qualified_type.type()});
    } else {
      NTH_UNIMPLEMENTED();
    }
  }

  std::reverse(parameters.begin(), parameters.end());
  for (size_t i = 0; i < return_types.size(); ++i) {
    context.type_stack().pop();
  }
  auto qt = type::QualifiedType::Constant(type::Function(
      type::Parameters(std::move(parameters)), std::move(return_types)));

  context.type_stack().push({qt});
  context.emit.SetQualifiedType(start, qt);
}

void HandleParseTreeNodeEnumLiteralStart(ParseNodeIndex, IrContext&,
                                         diag::DiagnosticConsumer&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeEnumLiteral(ParseNodeIndex, IrContext&,
                                    diag::DiagnosticConsumer&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeExtensionStart(ParseNodeIndex, IrContext&,
                                       diag::DiagnosticConsumer&) {}

Iteration HandleParseTreeNodeExtendWith(ParseNodeIndex index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer& diag) {
  std::span qts = context.type_stack().top();
  for (auto qt : qts) {
    if (qt.type() == type::Error) {
      context.type_stack().pop();
      context.type_stack().push(
          {type::QualifiedType::Unqualified(type::Error)});
      return Iteration::SkipTo(index + 2);
    }
  }

  if (not RequireConstant(qts[0], type::Type_, diag)) {
    context.MakeError(1);
    return Iteration::SkipTo(index + 2);
 }
 std::optional type = context.EvaluateAs<type::Type>(index - 1);
 if (not type) { NTH_UNIMPLEMENTED(); }
 // TODO: Register the extension.
 return Iteration::SkipTo(index + 2);
}
void HandleParseTreeNodeExtension(ParseNodeIndex, IrContext&,
                                  diag::DiagnosticConsumer&) {}

void HandleParseTreeNodeBinding(ParseNodeIndex index, IrContext&context,
                                diag::DiagnosticConsumer&diag) {
  context.declaration_stack().push({});
}

void HandleParseTreeNodePattern(ParseNodeIndex, IrContext&,
                                diag::DiagnosticConsumer&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeInterfaceLiteralStart(ParseNodeIndex, IrContext&,
                                              diag::DiagnosticConsumer&) {}

void HandleParseTreeNodeInterfaceLiteral(ParseNodeIndex, IrContext& context,
                                         diag::DiagnosticConsumer&) {
  context.type_stack().push(
      {type::QualifiedType::Constant(type::Pattern(type::Type_))});
}

void HandleParseTreeNodeWhileLoopStart(ParseNodeIndex index, IrContext& context,
                                       diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodeWhileLoopBodyStart(ParseNodeIndex index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {
  if (context.type_stack().top().size() != 1) {
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text("While-loop condition is not expanded."),
        diag::SourceQuote(context.Node(index).token),
    });
    context.MakeError(1);
  } else if (context.type_stack().top().size() != 1) {
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"While-loop conditions must be `bool`s, but you "
                              "provided a `{}`.">(
                context.type_stack().top()[0])),
        diag::SourceQuote(context.Node(index).token),
    });
    context.MakeError(1);
  } else {
    context.push_lexical_scope(context.Node(index).scope_index);
  }
}

void HandleParseTreeNodeWhileLoop(ParseNodeIndex index, IrContext& context,
                                  diag::DiagnosticConsumer&) {
  // TODO: This should have type-checking for all the statements that were
  // contained in it, esp. once we have early returns and yielding.
  context.type_stack().pop();
  context.type_stack().push({});
}

void HandleParseTreeNodeIfStatementTrueBranchStart(ParseNodeIndex index,
                                                   IrContext& context,
                                                   diag::DiagnosticConsumer&) {
  context.push_lexical_scope(context.Node(index).scope_index);
}

void HandleParseTreeNodeIfStatementFalseBranchStart(ParseNodeIndex index,
                                                    IrContext& context,
                                                    diag::DiagnosticConsumer&) {
  context.push_lexical_scope(context.Node(index).scope_index);
}

void HandleParseTreeNodeIfStatement(ParseNodeIndex index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  if (context.type_stack().top()[0].type() != type::Bool) {
    NTH_UNIMPLEMENTED("{}") <<= {context.type_stack().top()[0]};
  }
  context.pop_lexical_scope();
  context.type_stack().pop();
  context.type_stack().push({});
}

void HandleParseTreeNodeStatementStart(ParseNodeIndex index, IrContext& context,
                                       diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodeAssignedValueStart(ParseNodeIndex index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodeReturn(ParseNodeIndex index, IrContext& context,
                               diag::DiagnosticConsumer& diag) {
  // TODO: Check that the return type matches the signature.
}

void HandleParseTreeNodeFunctionLiteral(ParseNodeIndex index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer& diag) {
  context.queue.front().functions.pop_back();
  context.pop_lexical_scope();
  // TODO: Check that the return type matches the signature.
}

void HandleParseTreeNodeAssignment(ParseNodeIndex index, IrContext& context,
                                   diag::DiagnosticConsumer& diag) {
  auto rhs_qt = context.type_stack().top()[0];
  context.type_stack().pop();
  auto lhs_qt = context.type_stack().top()[0];

  if (lhs_qt.constant()) {
    NTH_UNIMPLEMENTED("TODO: Log error about assignign to a constant {}") <<=
        {lhs_qt};
  }
  // TODO: Check for assignability?

  if (not type::ImplicitCast(AnyValue::JustType(rhs_qt.type()),
                             lhs_qt.type())) {
    auto token = context.Node(index).token;
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(InterpolateString<
                   "Invalid assignment from type {} to object of type {}.">(
            rhs_qt.type(), lhs_qt.type())),
        diag::SourceQuote(token),
    });
  }
}

void HandleParseTreeNodeFunctionTypeParameters(ParseNodeIndex index,
                                               IrContext& context,
                                               diag::DiagnosticConsumer& diag) {
  for (size_t i = 0; i < context.emit.Node(index).child_count; ++i) {
    if (context.type_stack().top()[0] !=
        type::QualifiedType::Constant(type::Type_)) {
      NTH_UNIMPLEMENTED("{}") <<= {context.type_stack()};
    }
    context.type_stack().pop();
  }
  context.type_stack().push({type::QualifiedType::Constant(type::Type_)});
}

void HandleParseTreeNodeNoReturns(ParseNodeIndex index, IrContext& context,
                                  diag::DiagnosticConsumer& diag) {
  context.type_stack().push({});
}

template <auto F>
constexpr Iteration Invoke(ParseNodeIndex index, IrContext& context,
                           diag::DiagnosticConsumer& diag) {
  constexpr auto return_type =
      nth::type<std::invoke_result_t<decltype(F), ParseNodeIndex, IrContext&,
                                     diag::DiagnosticConsumer&>>;
  if constexpr (return_type == nth::type<Iteration>) {
    return F(index, context, diag);
  } else {
    F(index, context, diag);
    return Iteration::Continue;
  }
}

void ProcessIrImpl(IrContext& context, diag::DiagnosticConsumer& diag) {
  while (not context.queue.empty()) {
    auto item         = context.queue.front();
    auto [start, end] = item.interval;
    auto index        = start;
    for (; index != end; ++index) {
      switch (context.Node(index).kind) {
#define IC_XMACRO_PARSE_NODE(node_kind)                                        \
  case ParseNode::Kind::node_kind: {                                           \
    NTH_LOG((v.when(debug::type_check)), "Process node {} ({})") <<=           \
        {#node_kind, index};                                                   \
    auto it = Invoke<HandleParseTreeNode##node_kind>(index, context, diag);    \
    switch (it.kind()) {                                                       \
      case Iteration::PauseMoveOn: ++index; [[fallthrough]];                   \
      case Iteration::PauseRetry: goto stop_early;                             \
      case Iteration::Continue: break;                                         \
      case Iteration::Skip: index = it.index() - 1; break;                     \
    }                                                                          \
  } break;
#include "parse/node.xmacro.h"
      }
      context.identifier_repetition_counter = 0;
    }
    NTH_LOG((v.when(debug::type_check)), "Done type-checking chunk at {}") <<=
        {index};
    context.queue.pop();
    continue;

  stop_early:
    NTH_LOG((v.when(debug::type_check)), "Stopping early just before {}") <<=
        {index};
    ++context.identifier_repetition_counter;
    context.queue.pop();
  }
}

}  // namespace

void ProcessIr(EmitContext& emit, diag::DiagnosticConsumer& diag) {
  IrContext context{.emit = emit};
  context.queue.push(IrContext::WorkItem(emit.tree.node_range()));
  ProcessIrImpl(context, diag);
}

}  // namespace ic
