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
#include "ir/type_stack.h"
#include "jasmin/execute.h"
#include "jasmin/value_stack.h"
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
          c.type_stack().push({type::QualifiedType::Constant(type::Error)});   \
          return;                                                              \
        }                                                                      \
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

  TypeStack& type_stack() { return queue.front().type_stack_; }

  void PopTypeStack(size_t num_to_pop) {
    auto& ts = type_stack();
    for (size_t i = 0; i < num_to_pop; ++i) { ts.pop(); }
  }
  void MakeError(size_t num_to_pop) {
    PopTypeStack(num_to_pop);
    type_stack().push({type::QualifiedType::Unqualified(type::Error)});
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
    WorkItem(nth::interval<ParseNodeIndex> interval) : interval(interval) {}

    nth::interval<ParseNodeIndex> interval;
    std::vector<Token::Kind> operator_stack;
    std::vector<DeclarationInfo> declaration_stack;
    std::vector<Scope::Index> scopes;
    std::vector<Scope::Index> functions;

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
  context.pop_scope();
}

void HandleParseTreeNodeModuleStart(ParseNodeIndex index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  context.queue.front().functions.push_back(Scope::Index::Root());
  context.push_scope(Scope::Index::Root());
}

void HandleParseTreeNodeDeclaration(ParseNodeIndex index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  DeclarationInfo info = context.declaration_stack().back();
  context.declaration_stack().pop_back();
  if (not info.kind.has_initializer()) {
    NTH_REQUIRE((v.debug), not info.kind.inferred_type());
    context.type_stack().pop();
    std::optional type = context.EvaluateAs<type::Type>(index - 1);
    if (not type) { NTH_UNIMPLEMENTED(); }

    auto qt = info.kind.constant() ? type::QualifiedType::Constant(*type)
                                   : type::QualifiedType::Unqualified(*type);
    context.current_scope().insert_identifier(
        context.Node(info.index).token.Identifier(),
        {
            .declaration    = info.index,
            .identifier     = index,
            .qualified_type = qt,
        });

    context.current_storage().insert(index, *type);
    context.emit.SetQualifiedType(index, qt);
  } else if (info.kind.inferred_type()) {
    type::QualifiedType qt;
    if (info.kind.constant()) {
      qt = type::QualifiedType::Constant(context.type_stack().top()[0].type());
    } else {
      qt = type::QualifiedType::Unqualified(context.type_stack().top()[0].type());
      context.current_storage().insert(index, qt.type());
    }

    context.current_scope().insert_identifier(
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
    if (not type::ImplicitCast(init_qt.type(), *type)) {
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
    context.current_scope().insert_identifier(
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
  if (auto* decl_info =
          context.emit.scopes.identifier(context.current_scope_index(), id)) {
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
  IC_PROPAGATE_ERRORS(context, node, node.child_count / 2);
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
      type::Type return_type = context.type_stack().top()[0].type();
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

      context.type_stack().pop();
      context.type_stack().push({type::QualifiedType::Constant(type::Type_)});
    } break;
    case Token::Kind::Plus: 
    case Token::Kind::Minus:
    case Token::Kind::Percent:
    case Token::Kind::Star: {
      NTH_REQUIRE(context.type_stack().group_count() >= node.child_count / 2);
      auto iter = context.type_stack().rbegin();
      std::vector<type::QualifiedType> types;
      for (size_t i = 0; i <= node.child_count / 2; ++i, ++iter) {
        if ((*iter).size() != 1) { NTH_UNIMPLEMENTED(); }
        types.push_back((*iter)[0]);
      }
      for (size_t i = 0; i + 1 < types.size(); ++i) {
        if (types[i] != types[i + 1]) { NTH_UNIMPLEMENTED(); }
      }
      context.PopTypeStack(1 + node.child_count / 2);
      context.type_stack().push({types[0]});
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
        if (types[i] != types[i + 1]) { NTH_UNIMPLEMENTED(); }
      }
      context.PopTypeStack(1 + node.child_count / 2);
      context.type_stack().push({type::QualifiedType::Constant(type::Bool)});
    } break;
    default: NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeDeclarationStart(ParseNodeIndex index, IrContext& context,
                                         diag::DiagnosticConsumer&) {
  context.declaration_stack().emplace_back() =
      context.Node(index).declaration_info;
}

void HandleParseTreeNodeMemberExpression(ParseNodeIndex index,
                                         IrContext& context,
                                         diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  IC_PROPAGATE_ERRORS(context, node, 1);
  NTH_REQUIRE(not context.type_stack().empty());
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
      auto qt                     = type::QualifiedType::Unqualified(type::U64);
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

void HandleParseTreeNodeCallExpression(ParseNodeIndex index, IrContext& context,
                                       diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  IC_PROPAGATE_ERRORS(context, node, node.child_count - 1);

  auto iter = context.type_stack().rbegin();
  for (size_t i = 2; i < node.child_count; ++i) {
    NTH_REQUIRE((v.debug), iter != context.type_stack().rend());
    ++iter;
  }
  auto invocable_type = (*iter)[0];
  if (invocable_type.type().kind() == type::Type::Kind::Function) {
    auto fn_type           = invocable_type.type().AsFunction();
    auto const& parameters = *fn_type.parameters();
    // TODO: Properly implement function call type-checking.
    if (parameters.size() == node.child_count - 2) {
      auto type_iter             = context.type_stack().rbegin();
      auto& argument_width_count = context.emit.rotation_count[index];
      for (size_t i = 0; i < parameters.size(); ++i) {
        argument_width_count += type::JasminSize((*type_iter)[0].type());
        ++type_iter;
      }
      auto const& returns = fn_type.returns();
      for (size_t i = 1; i < node.child_count; ++i) {
        context.type_stack().pop();
      }
      std::vector<type::QualifiedType> return_qts;
      for (type::Type r : returns) {
        return_qts.push_back(type::QualifiedType::Unqualified(r));
      }
      context.type_stack().push(return_qts);

      switch (fn_type.evaluation()) {
        case type::Evaluation::PreferCompileTime: NTH_UNIMPLEMENTED();
        case type::Evaluation::RequireCompileTime: {
          nth::interval range = context.emit.tree.subtree_range(index);
          jasmin::ValueStack value_stack;
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
    } else {
      NTH_UNIMPLEMENTED("{} {}") <<= {parameters, node.child_count - 1};
    }
  } else if (invocable_type.type().kind() ==
             type::Type::Kind::GenericFunction) {
    auto& rotation_count = context.emit.rotation_count[index];
    std::vector<ParseNodeIndex> argument_indices;
    for (auto iter = context.ChildIndices(index).begin();
         context.Node(*iter).kind != ParseNode::Kind::InvocationArgumentStart;
         ++iter) {
      argument_indices.push_back(*iter);
    }

    std::reverse(argument_indices.begin(), argument_indices.end());
    jasmin::ValueStack value_stack;

    auto iter = context.type_stack().rbegin();
    for (size_t i = 1; i < argument_indices.size(); ++i) {
      NTH_REQUIRE((v.debug), iter != context.type_stack().rend());
      ++iter;
    }
    for (auto index : argument_indices) {
      auto t              = (*iter)[0].type();

      nth::interval range = context.emit.tree.subtree_range(index);
      context.emit.Evaluate(range, value_stack, {t});
      rotation_count += type::JasminSize(t);
      ++iter;
    }
    auto g = invocable_type.type().AsGenericFunction();
    jasmin::Execute(*static_cast<IrFunction const*>(g.data()), value_stack);
    auto t = value_stack.pop<type::Type>();
    context.type_stack().push({type::QualifiedType::Constant(t)});
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
  if (context.type_stack().top()[0].type() != type::Type_) {
    NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeBufferPointer(ParseNodeIndex index, IrContext& context,
                                      diag::DiagnosticConsumer& diag) {
  if (context.type_stack().top()[0].type() != type::Type_) {
    NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeSlice(ParseNodeIndex index, IrContext& context,
                              diag::DiagnosticConsumer& diag) {
  if (context.type_stack().top()[0].type() != type::Type_) {
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
    context.type_stack().pop();
    context.type_stack().push({type::QualifiedType::Constant(type::Error)});
    return;
  }

  context.type_stack().pop();
  context.type_stack().push({type::QualifiedType::Constant(type::Module)});
}

Iteration HandleParseTreeNodeScopeStart(ParseNodeIndex index, IrContext& context,
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
  Scope::Index scope_index = context.Node(index).scope_index;
  context.queue.front().functions.push_back(scope_index);
  context.push_scope(scope_index);
}

void HandleParseTreeNodeFunctionLiteralSignature(
    ParseNodeIndex index, IrContext& context, diag::DiagnosticConsumer& diag) {
  ParseNodeIndex start = context.emit.tree.first_descendant_index(index) - 1;
  NTH_REQUIRE((v.debug), context.Node(start).kind ==
                             ParseNode::Kind::FunctionLiteralStart);
  std::vector<type::Type> return_types;
  std::vector<type::ParametersType::Parameter> parameters;
  auto indices = context.ChildIndices(index);

  auto iter    = indices.begin();
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

    if (auto* decl_info =
            context.current_scope().identifier(decl_iter->token.Identifier())) {
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

void HandleParseTreeNodeIfStatementTrueBranchStart(ParseNodeIndex index,
                                                   IrContext& context,
                                                   diag::DiagnosticConsumer&) {
  context.push_scope(context.Node(index).scope_index);
}

void HandleParseTreeNodeIfStatementFalseBranchStart(ParseNodeIndex index,
                                                    IrContext& context,
                                                    diag::DiagnosticConsumer&) {
  context.push_scope(context.Node(index).scope_index);
}

void HandleParseTreeNodeIfStatement(ParseNodeIndex index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  if (context.type_stack().top()[0].type() != type::Bool) {
    NTH_UNIMPLEMENTED();
  }
  context.pop_scope();
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
  context.pop_scope();
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

  if (not type::ImplicitCast(rhs_qt.type(), lhs_qt.type())) {
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
    switch (                                                                   \
        Invoke<HandleParseTreeNode##node_kind>(index, context, diag).kind()) { \
      case Iteration::PauseMoveOn: ++index; [[fallthrough]];                   \
      case Iteration::PauseRetry: goto stop_early;                             \
      case Iteration::Continue: break;                                         \
      case Iteration::Skip: NTH_UNREACHABLE();                                 \
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
