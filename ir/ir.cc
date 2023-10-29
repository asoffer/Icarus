#include "ir/ir.h"

#include <optional>
#include <span>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "common/debug.h"
#include "common/module_id.h"
#include "common/resources.h"
#include "common/string.h"
#include "jasmin/execute.h"
#include "jasmin/value_stack.h"
#include "nth/debug/debug.h"
#include "parser/parse_tree.h"
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
  ParseTree::Node const& Node(ParseTree::Node::Index index) {
    return emit.tree[index];
  }

  auto ChildIndices(ParseTree::Node::Index index) {
    return emit.tree.child_indices(index);
  }
  auto Children(ParseTree::Node::Index index) {
    return emit.tree.children(index);
  }

  template <typename T>
  std::optional<T> EvaluateAs(ParseTree::Node::Index subtree_root_index) {
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

  struct WorkItem {
    nth::interval<ParseTree::Node::Index> interval;
    std::vector<type::QualifiedType> type_stack;
    std::vector<Token::Kind> operator_stack;
    std::vector<DeclarationInfo> declaration_stack;
  };

  std::queue<WorkItem> queue;
  EmitContext& emit;
};

#define IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND(name, t)                       \
  void HandleParseTreeNode##name(ParseTree::Node::Index index,                 \
                                 IrContext& context,                           \
                                 diag::DiagnosticConsumer& diag) {             \
    auto qt = type::QualifiedType::Constant(t);                                \
    context.type_stack().push_back(qt);                                        \
    context.emit.SetQualifiedType(index, qt);                                  \
  }
#include "parser/parse_tree_node_kind.xmacro.h"

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

void HandleParseTreeNodeDeclaration(ParseTree::Node::Index index,
                                    IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  DeclarationInfo info = context.declaration_stack().back();
  context.declaration_stack().pop_back();
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
    } break;
    case Token::Kind::ColonColon: NTH_UNIMPLEMENTED(); break;
    case Token::Kind::ColonEqual: NTH_UNIMPLEMENTED(); break;
    case Token::Kind::ColonColonEqual: {
      // TODO: Other qualifiers?
      type::QualifiedType qt =
          type::QualifiedType::Constant(context.type_stack().back().type());
      context.emit.identifiers.emplace(
          context.Node(info.index).token.Identifier(),
          std::tuple(info.index, index, qt));
      context.type_stack().pop_back();
    } break;
    default: NTH_UNREACHABLE();
  }
}

void HandleParseTreeNodeStatement(ParseTree::Node::Index index,
                                  IrContext& context,
                                  diag::DiagnosticConsumer& diag) {
  context.emit.statement_qualified_type.emplace(index,
                                                context.type_stack().back());
  context.type_stack().pop_back();
}

void HandleParseTreeNodeStatementSequence(ParseTree::Node::Index index,
                                          IrContext& context,
                                          diag::DiagnosticConsumer& diag) {}

bool HandleParseTreeNodeIdentifier(ParseTree::Node::Index index,
                                   IrContext& context,
                                   diag::DiagnosticConsumer& diag) {
  auto iter =
      context.emit.identifiers.find(context.Node(index).token.Identifier());
  if (iter == context.emit.identifiers.end()) {
    auto item     = context.queue.front();
    item.interval = nth::interval(index, item.interval.upper_bound());
    context.queue.push(std::move(item));
    return false;
  }
  auto const& [decl_id_index, decl_index, decl_qt] = iter->second;
  context.emit.declarator.emplace(index, std::pair{decl_id_index, decl_index});
  context.type_stack().push_back(decl_qt);
  return true;
}

void HandleParseTreeNodeInfixOperator(ParseTree::Node::Index index,
                                      IrContext& context,
                                      diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  context.operator_stack().push_back(node.token.kind());
}

void HandleParseTreeNodeExpressionPrecedenceGroup(
    ParseTree::Node::Index index, IrContext& context,
    diag::DiagnosticConsumer& diag) {
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

void HandleParseTreeNodeLet(ParseTree::Node::Index, IrContext& context,
                            diag::DiagnosticConsumer&) {
  context.declaration_stack().emplace_back();
}

void HandleParseTreeNodeVar(ParseTree::Node::Index, IrContext& context,
                            diag::DiagnosticConsumer&) {
  context.declaration_stack().emplace_back();
}

void HandleParseTreeNodeColonColonEqual(ParseTree::Node::Index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer&) {
  context.declaration_stack().back().kind = Token::Kind::ColonColonEqual;
}

void HandleParseTreeNodeColonEqual(ParseTree::Node::Index, IrContext& context,
                                   diag::DiagnosticConsumer&) {
  context.declaration_stack().back().kind = Token::Kind::ColonEqual;
}

void HandleParseTreeNodeColonColon(ParseTree::Node::Index, IrContext& context,
                                   diag::DiagnosticConsumer&) {
  context.declaration_stack().back().kind = Token::Kind::ColonColon;
}

void HandleParseTreeNodeColon(ParseTree::Node::Index, IrContext& context,
                              diag::DiagnosticConsumer&) {
  context.declaration_stack().back().kind = Token::Kind::Colon;
}

void HandleParseTreeNodeExpressionGroup(ParseTree::Node::Index index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer& diag) {
  // Nothing to do here.
}

void HandleParseTreeNodeMemberExpression(ParseTree::Node::Index index,
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

void HandleParseTreeNodeCallExpression(ParseTree::Node::Index index,
                                       IrContext& context,
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
    std::vector<ParseTree::Node::Index> indices;
    for (auto iter = context.ChildIndices(index).begin();
         context.Node(*iter).kind !=
         ParseTree::Node::Kind::InvocationArgumentStart;
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

void HandleParseTreeNodeDeclaredIdentifier(ParseTree::Node::Index index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {
  context.declaration_stack().back().index = index;
}

void HandleParseTreeNodeInvocationArgumentStart(
    ParseTree::Node::Index index, IrContext& context,
    diag::DiagnosticConsumer& diag) {}

void HandleParseTreeNodePointer(ParseTree::Node::Index index,
                                IrContext& context,
                                diag::DiagnosticConsumer& diag) {
  if (context.type_stack().back().type() != type::Type_) {
    NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeBufferPointer(ParseTree::Node::Index index,
                                      IrContext& context,
                                      diag::DiagnosticConsumer& diag) {
  if (context.type_stack().back().type() != type::Type_) {
    NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeImport(ParseTree::Node::Index index, IrContext& context,
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

bool HandleParseTreeNodeScopeStart(ParseTree::Node::Index index,
                                   IrContext& context,
                                   diag::DiagnosticConsumer& diag) {
  auto item         = std::move(context.queue.front());
  auto [start, end] = item.interval;
  NTH_REQUIRE((v.harden), start == index);
  for (auto i : context.emit.tree.child_indices(
           context.Node(index).next_sibling_index)) {
    item.interval = context.emit.tree.subtree_range(i);
    context.queue.push(item);
  }

  return false;
}

void HandleParseTreeNodeFunctionTypeParameters(ParseTree::Node::Index index,
                                               IrContext& context,
                                               diag::DiagnosticConsumer& diag) {
  for (size_t i = 0; i < context.emit.Node(index).child_count; ++i) {
    if (context.type_stack().back() !=
        type::QualifiedType::Constant(type::Type_)) {
      NTH_UNIMPLEMENTED("{}") <<={context.type_stack()};
    }
    context.type_stack().pop_back();
  }
  context.type_stack().push_back(type::QualifiedType::Constant(type::Type_));
}

template <auto F>
constexpr bool Invoke(ParseTree::Node::Index index, IrContext& context,
                      diag::DiagnosticConsumer& diag) {
  constexpr auto return_type =
      nth::type<std::invoke_result_t<decltype(F), ParseTree::Node::Index,
                                     IrContext&, diag::DiagnosticConsumer&>>;
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
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind)                                   \
  case ParseTree::Node::Kind::kind: {                                          \
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
#include "parser/parse_tree_node_kind.xmacro.h"
      }
    }
  next_chunk:;
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
