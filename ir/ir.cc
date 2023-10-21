#include "ir/ir.h"

#include "common/module_id.h"
#include "common/string.h"
#include "jasmin/execute.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"
#include "type/type.h"

namespace ic {
namespace {

#define IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND(name, t)                       \
  void HandleParseTreeNode##name(ParseTree::Node::Index index,                 \
                                 IrContext& context,                           \
                                 diag::DiagnosticConsumer& diag) {             \
    context.type_stack.push_back(type::QualifiedType::Constant(t));            \
  }
#include "parser/parse_tree_node_kind.xmacro.h"

void HandleParseTreeNodeDeclaration(ParseTree::Node::Index index,
                                    IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  DeclarationInfo info = context.declaration_stack.back();
  context.declaration_stack.pop_back();
  switch (info.kind) {
    case Token::Kind::Colon: NTH_UNIMPLEMENTED(); break;
    case Token::Kind::ColonColon: NTH_UNIMPLEMENTED(); break;
    case Token::Kind::ColonEqual: NTH_UNIMPLEMENTED(); break;
    case Token::Kind::ColonColonEqual: {
      type::QualifiedType qt = context.type_stack.back();
      context.emit.identifiers.emplace(
          context.Node(info.index).token.IdentifierIndex(),
          std::pair(info.index, qt));
      context.type_stack.pop_back();
    } break;
    default: NTH_UNREACHABLE();
  }
}

void HandleParseTreeNodeStatementSequence(ParseTree::Node::Index index,
                                          IrContext& context,
                                          diag::DiagnosticConsumer& diag) {
  switch (context.Node(index - 1).kind) {
    case ParseTree::Node::Kind::Declaration: return;
    default:
      NTH_REQUIRE(not context.type_stack.empty());
      auto node = context.Node(index);
      context.emit.statement_qualified_type.emplace(index,
                                                    context.type_stack.back());
  }
}

void HandleParseTreeNodeIdentifier(ParseTree::Node::Index index,
                                   IrContext& context,
                                   diag::DiagnosticConsumer& diag) {
  auto iter = context.emit.identifiers.find(
      context.Node(index).token.IdentifierIndex());
  if (iter == context.emit.identifiers.end()) {
    NTH_UNIMPLEMENTED("{}") <<= {context.Node(index).token};
  }
  auto const& [decl_index, decl_qt] = iter->second;
  context.emit.declarator.emplace(index, decl_index);
  context.type_stack.push_back(decl_qt);
}

void HandleParseTreeNodeInfixOperator(ParseTree::Node::Index index,
                                      IrContext& context,
                                      diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  context.operator_stack.push_back(node.token.kind());
}

void HandleParseTreeNodeExpressionPrecedenceGroup(
    ParseTree::Node::Index index, IrContext& context,
    diag::DiagnosticConsumer& diag) {
  Token::Kind kind = context.operator_stack.back();
  context.operator_stack.pop_back();
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
      NTH_REQUIRE(context.type_stack.size() >= 2);
      type::Type return_type = context.type_stack.back().type();
      context.type_stack.pop_back();
      type::Type parameters_type = context.type_stack.back().type();
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

      context.type_stack.back() =
          type::QualifiedType(type::Qualifier::Constant(), type::Type_);
    } break;
    default: NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeLet(ParseTree::Node::Index, IrContext& context,
                            diag::DiagnosticConsumer&) {
  context.declaration_stack.emplace_back();
}

void HandleParseTreeNodeVar(ParseTree::Node::Index, IrContext& context,
                            diag::DiagnosticConsumer&) {
  context.declaration_stack.emplace_back();
}

void HandleParseTreeNodeColonColonEqual(ParseTree::Node::Index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer&) {
  context.declaration_stack.back().kind = Token::Kind::ColonColonEqual;
}

void HandleParseTreeNodeColonEqual(ParseTree::Node::Index, IrContext& context,
                                   diag::DiagnosticConsumer&) {
  context.declaration_stack.back().kind = Token::Kind::ColonEqual;
}

void HandleParseTreeNodeColonColon(ParseTree::Node::Index, IrContext& context,
                                   diag::DiagnosticConsumer&) {
  context.declaration_stack.back().kind = Token::Kind::ColonColon;
}

void HandleParseTreeNodeColon(ParseTree::Node::Index, IrContext& context,
                              diag::DiagnosticConsumer&) {
  context.declaration_stack.back().kind = Token::Kind::Colon;
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
  NTH_REQUIRE(not context.type_stack.empty());
  if (context.type_stack.back() ==
      type::QualifiedType::Constant(type::Module)) {
    auto module_id = context.EvaluateAs<ModuleId>(index - 1);
    NTH_REQUIRE(module_id.has_value());
    context.type_stack.back() = context.emit.module(*module_id)
                                    .Lookup(node.token.IdentifierIndex())
                                    .qualified_type;
    if (context.type_stack.back().type() == type::Error) {
      diag.Consume({
          diag::Header(diag::MessageKind::Error),
          diag::Text(
              InterpolateString<"No symbol named '{}' in the given module.">(
                  diag.Symbol(context.Node(index).token))),
          diag::SourceQuote(context.Node(index - 1).token),
      });
    }
  } else if (context.type_stack.back().type() == type::Type_) {
    NTH_UNIMPLEMENTED("{} -> {}") <<= {context.type_stack.back(), node.token};
  } else {
    diag.Consume({
        diag::Header(diag::MessageKind::Error),
        diag::Text(
            InterpolateString<"Access operator `.` may only follow a type, "
                              "module, or enum, but you provided: {}.">(
                context.type_stack.back().type())),
        diag::SourceQuote(context.Node(index - 2).token),
    });
    context.type_stack.back().type() = type::Error;
  }
}

#define IC_PROPAGATE_ERRORS(context, node)                                     \
  do {                                                                         \
    auto&& c = (context);                                                      \
    auto&& n = (node);                                                         \
    for (auto const* p = &c.type_stack[c.type_stack.size() - n.child_count];   \
         p != &c.type_stack.back(); ++p) {                                     \
      if (p->type() == type::Error) {                                          \
        c.type_stack.resize(c.type_stack.size() - n.child_count + 1);          \
        c.type_stack.back() = type::QualifiedType::Constant(type::Error);      \
        return;                                                                \
      }                                                                        \
    }                                                                          \
  } while (false);

void HandleParseTreeNodeCallExpression(ParseTree::Node::Index index,
                                       IrContext& context,
                                       diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  IC_PROPAGATE_ERRORS(context, node);

  auto invocable_type =
      context.type_stack[context.type_stack.size() - node.child_count];
  if (invocable_type.type().kind() == type::Type::Kind::Function) {
    auto fn_type           = invocable_type.type().AsFunction();
    auto const& parameters = *fn_type.parameters();
    // TODO: Properly implement function call type-checking.
    if (parameters.size() == node.child_count - 1) {
      auto type_iter             = context.type_stack.rbegin();
      auto& argument_width_count = context.emit.rotation_count[index];
      for (size_t i = 0; i < parameters.size(); ++i) {
        argument_width_count += type::JasminSize(type_iter->type());
        ++type_iter;
      }
      auto const& returns = fn_type.returns();
      context.type_stack.resize(context.type_stack.size() - node.child_count);
      for (type::Type r : returns) {
        context.type_stack.push_back(
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
      NTH_UNIMPLEMENTED();
    }
  } else if (invocable_type.type().kind() ==
             type::Type::Kind::GenericFunction) {
    auto& rotation_count    = context.emit.rotation_count[index];
    std::vector<ParseTree::Node::Index> indices;
    for (auto iter = context.ChildIndices(index).begin();
         context.Node(*iter).kind !=
         ParseTree::Node::Kind::InvocationArgumentStart;
         ++iter) {
      indices.push_back(*iter);
    }
    size_t type_stack_index = context.type_stack.size() - indices.size();
    std::reverse(indices.begin(), indices.end());
    jasmin::ValueStack value_stack;
    for (auto index : indices) {
      auto t = context.type_stack[type_stack_index].type();
      nth::interval range = context.emit.tree.subtree_range(index);
      context.emit.Evaluate(range, value_stack, {t});
      rotation_count += type::JasminSize(t);
      ++type_stack_index;
    }
    auto g = invocable_type.type().AsGenericFunction();
    jasmin::Execute(*static_cast<IrFunction const*>(g.data()), value_stack);
    auto t = value_stack.pop<type::Type>();
    context.type_stack.push_back(type::QualifiedType::Constant(t));
    NTH_REQUIRE((v.debug), t.kind() == type::Type::Kind::Function);

    if (g.evaluation() == type::Evaluation::PreferCompileTime or
        g.evaluation() == type::Evaluation::RequireCompileTime) {
      jasmin::ValueStack value_stack;
      context.emit.Evaluate(context.emit.tree.subtree_range(index), value_stack,
                            {t});
    }
  } else {
    NTH_UNIMPLEMENTED("{}") <<= {node};
  }
}

void HandleParseTreeNodeDeclaredIdentifier(ParseTree::Node::Index index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {
  context.declaration_stack.back().index = index;
}

void HandleParseTreeNodeInvocationArgumentStart(
    ParseTree::Node::Index index, IrContext& context,
    diag::DiagnosticConsumer& diag) {}

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
    context.type_stack.back() = type::QualifiedType::Constant(type::Error);
    return;
  }

  context.type_stack.back() = type::QualifiedType::Constant(type::Module);
}

void HandleParseTreeNodeScopeStart(ParseTree::Node::Index index,
                                   IrContext& context,
                                   diag::DiagnosticConsumer& diag) {}

}  // namespace

void IrContext::ProcessIr(diag::DiagnosticConsumer& diag) {
  for (size_t i = 0; i < emit.tree.nodes().size(); ++i) {
    ParseTree::Node::Index index(i);
    auto node = Node(index);
    switch (node.kind) {
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind)                                   \
  case ParseTree::Node::Kind::kind:                                            \
    NTH_LOG((v.when(false)), "Process node {}") <<= {#kind};                   \
    HandleParseTreeNode##kind(ParseTree::Node::Index(i), *this, diag);         \
    break;
#include "parser/parse_tree_node_kind.xmacro.h"
    }
  }
}

}  // namespace ic
