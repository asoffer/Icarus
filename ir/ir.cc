#include "ir/ir.h"

#include "common/string.h"
#include "ir/module_id.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"
#include "type/type.h"

namespace ic {
namespace {

#define IC_XMACRO_PARSE_TREE_NODE_CONSTANT_KIND(name, t)                       \
  void HandleParseTreeNode##name(ParseTree::Node::Index index,                 \
                                 IrContext& context,                           \
                                 diag::DiagnosticConsumer& diag) {             \
    context.type_stack.push_back(                                              \
        type::QualifiedType(type::Qualifier::Constant(), t));                  \
 }
#include "parser/parse_tree_node_kind.xmacro.h"

void HandleParseTreeNodeDeclaration(ParseTree::Node::Index index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  context.identifiers.emplace(node.token.IdentifierIndex(),
                              context.type_stack.back());
  context.type_stack.pop_back();
}

void HandleParseTreeNodeStatementSequence(ParseTree::Node::Index index,
                                          IrContext& context,
                                          diag::DiagnosticConsumer& diag) {
  NTH_REQUIRE(not context.type_stack.empty());
  auto node = context.Node(index);
  context.emit.statement_qualified_type.emplace(node,
                                                context.type_stack.back());
}

void HandleParseTreeNodeIdentifier(ParseTree::Node::Index index,
                                   IrContext& context,
                                   diag::DiagnosticConsumer& diag) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeInfixOperator(ParseTree::Node::Index index, IrContext& context,
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
      if (node.child_count != 2) {
        NTH_REQUIRE(node.child_count != -1);
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

void HandleParseTreeNodeLet(ParseTree::Node::Index, IrContext&,
                            diag::DiagnosticConsumer&) {
  // TODO: Not handled yet.
}

void HandleParseTreeNodeVar(ParseTree::Node::Index, IrContext&,
                            diag::DiagnosticConsumer&) {
  // TODO: Not handled yet.
}

void HandleParseTreeNodeColonColonEqual(ParseTree::Node::Index, IrContext&,
                                        diag::DiagnosticConsumer&) {
  // TODO: Not handled yet.
}

void HandleParseTreeNodeColonEqual(ParseTree::Node::Index, IrContext&,
                                   diag::DiagnosticConsumer&) {
  // TODO: Not handled yet.
}

void HandleParseTreeNodeColonColon(ParseTree::Node::Index, IrContext&,
                                        diag::DiagnosticConsumer&) {
  // TODO: Not handled yet.
}

void HandleParseTreeNodeColon(ParseTree::Node::Index, IrContext&,
                                   diag::DiagnosticConsumer&) {
  // TODO: Not handled yet.
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
      type::QualifiedType(type::Qualifier::Constant(), type::Module)) {
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
          diag::SourceQuote(context.Node(index - 2).token),
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

void HandleParseTreeNodeCallExpression(ParseTree::Node::Index index,
                                       IrContext& context,
                                       diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  auto invocable_type =
      context.type_stack[context.type_stack.size() - node.child_count];
  if (invocable_type.type().kind() == type::Type::Kind::Function) {
    auto fn_type = invocable_type.type().AsFunction();
    auto const & parameters = *fn_type.parameters();
    // TODO: Properly implement function call type-checking.
    if (parameters.size() == node.child_count - 1) {
      auto const& returns = fn_type.returns();
      context.type_stack.resize(context.type_stack.size() - node.child_count);
      for (type::Type r : returns) {
        context.type_stack.push_back(
            type::QualifiedType(type::Qualifier::Unqualified(), r));
      }
    } else {
      NTH_UNIMPLEMENTED();
    }
  } else {
    NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeDeclaredIdentifier(ParseTree::Node::Index index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {
  NTH_UNIMPLEMENTED("{}") <<= {context.Node(index)};
}

}  // namespace

void IrContext::ProcessIr(diag::DiagnosticConsumer& diag) {
  for (size_t i = 0; i < emit.tree.nodes().size(); ++i) {
    ParseTree::Node::Index index(i);
    auto node = Node(index);
    switch (node.kind) {
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind)                                   \
  case ParseTree::Node::Kind::kind:                                            \
    NTH_LOG((v.when(false)), "Parse node {}") <<= {#kind};                     \
    HandleParseTreeNode##kind(ParseTree::Node::Index(i), *this, diag);         \
    break;
#include "parser/parse_tree_node_kind.xmacro.h"
    }
  }
}

}  // namespace ic
