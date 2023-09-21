#include "ir/ir.h"

#include "common/string.h"
#include "ir/module_id.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"
#include "type/type.h"

namespace ic {
namespace {

void HandleParseTreeNodeBooleanLiteral(ParseTree::Node::Index index, IrContext& context,
                                       diag::DiagnosticConsumer& diag) {
  context.type_stack.push_back(type::Bool);
}

void HandleParseTreeNodeIntegerLiteral(ParseTree::Node::Index index, IrContext& context,
                                       diag::DiagnosticConsumer& diag) {
  context.type_stack.push_back(type::Integer);
}

void HandleParseTreeNodeTypeLiteral(ParseTree::Node::Index index, IrContext& context,
                                    diag::DiagnosticConsumer& diag) {
  context.type_stack.push_back(type::Type_);
}

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
  auto node = context.Node(index);
  context.emit.statement_type.emplace(node, context.type_stack.back());
}

void HandleParseTreeNodeIdentifier(ParseTree::Node::Index index,
                                   IrContext& context,
                                   diag::DiagnosticConsumer& diag) {
  auto node = context.Node(index);
  if (not context.operator_stack.empty() and
      context.operator_stack.back() == Token::Kind::Period) {
    NTH_REQUIRE(not context.type_stack.empty());
    if (context.type_stack.back() == type::Module) {
      auto module_id = context.EvaluateAs<ModuleId>(index - 2);
      NTH_REQUIRE(module_id.has_value());
      context.type_stack.back() =
          context.module(*module_id).Lookup(node.token.IdentifierIndex()).type;
    } else if (context.type_stack.back() == type::Type_) {
      NTH_UNIMPLEMENTED("{} -> {}") <<= {context.type_stack.back(), node.token};
    } else {
      diag.Consume({
          diag::Header(diag::MessageKind::Error),
          diag::Text(
              InterpolateString<"Access operator `.` may only follow a type, "
                                "module, or enum, but you provided: {}.">(
                  context.type_stack.back())),
          diag::SourceQuote(context.Node(index - 2).token),
      });
      context.type_stack.back() = type::Error;
    }
  } else {
    NTH_UNIMPLEMENTED("{}") <<= {node};
  }
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
      type::Type return_type = context.type_stack.back();
      context.type_stack.pop_back();
      type::Type parameters_type = context.type_stack.back();
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

      context.type_stack.back() = type::Type_;
    } break;
    default: NTH_UNIMPLEMENTED();
  }
}

void HandleParseTreeNodeExpressionGroup(ParseTree::Node::Index index,
                                        IrContext& context,
                                        diag::DiagnosticConsumer& diag) {
  // Nothing to do here.
}

void HandleParseTreeNodeBuiltin(ParseTree::Node::Index index,
                                IrContext& context,
                                diag::DiagnosticConsumer& diag) {
  context.type_stack.push_back(type::Module);
}

void HandleParseTreeNodeCallArgumentsStart(ParseTree::Node::Index index,
                                           IrContext& context,
                                           diag::DiagnosticConsumer& diag) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeCallExpression(ParseTree::Node::Index index,
                                       IrContext& context,
                                       diag::DiagnosticConsumer& diag) {
  NTH_UNIMPLEMENTED();
}

}  // namespace

void IrContext::ProcessIr(diag::DiagnosticConsumer& diag) {
  for (size_t i = 0; i < tree.nodes().size(); ++i) {
    auto node = Node(ParseTree::Node::Index(i));
    switch (node.kind) {
#define IC_XMACRO_PARSE_TREE_NODE_KIND(kind)                                   \
  case ParseTree::Node::Kind::kind:                                            \
    NTH_LOG((v.always), "Parse node {}") <<= {#kind};                          \
    HandleParseTreeNode##kind(ParseTree::Node::Index(i), *this, diag);         \
    break;
#include "parser/parse_tree_node_kind.xmacro.h"
    }
  }
}

}  // namespace ic
