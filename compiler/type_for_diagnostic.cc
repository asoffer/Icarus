#include "compiler/type_for_diagnostic.h"

#include <sstream>

#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "ast/visitor.h"

namespace compiler {
namespace {

enum class VisitationKind { Type, ReturnType };

struct StringifyExpression : ast::Visitor<std::string()> {
  explicit StringifyExpression(Context const *c, VisitationKind kind)
      : context_(*ASSERT_NOT_NULL(c)), kind_(kind) {}

  std::string Visit(ast::Expression const *node) {
    return ast::Visitor<std::string()>::Visit(node);
  }

  std::string Visit(ast::Access const *node) final {
    return absl::StrCat(Visit(node->operand()), ".", node->member_name());
  }

  std::string Visit(ast::ArrayType const *node) final {
    return absl::StrCat(
        "[",
        absl::StrJoin(node->lengths(), ", ",
                      [&](std::string *out, auto const *length_expr) {
                        absl::StrAppend(out, Visit(length_expr));
                      }),
        "; ", Visit(node->data_type()), "]");
  }

  std::string Visit(ast::BindingDeclaration const *node) final {
    return std::string(node->ids()[0].name());
  }

  std::string Visit(ast::Call const *node) final {
    return absl::StrCat(
        Visit(node->callee()), "(",
        absl::StrJoin(node->arguments(), ", ",
                      [&](std::string *out, auto const &arg) {
                        if (arg.named()) {
                          absl::StrAppend(out, arg.name(), " = ",
                                          Visit(&arg.expr()));
                        } else {
                          absl::StrAppend(out, Visit(&arg.expr()));
                        }
                      }),
        ")");
  }

  std::string Visit(ast::Identifier const *node) final {
    return std::string(node->name());
  }

  std::string Visit(ast::PatternMatch const *node) final {
    return Visit(&node->pattern());
  }

  std::string Visit(ast::Terminal const *node) final {
    std::stringstream ss;
    context_.qual_types(node)[0].type().ShowValue(ss, node->value());
    return ss.str();
  }

  std::string Visit(ast::UnaryOperator const *node) final;

 private:
  Context const &context_;
  VisitationKind kind_ = VisitationKind::Type;
};

struct StringifyType : ast::Visitor<std::string()> {
  explicit StringifyType(Context const *c, VisitationKind kind)
      : context_(*ASSERT_NOT_NULL(c)), kind_(kind) {}

  std::string Visit(ast::Expression const *node) {
    return ast::Visitor<std::string()>::Visit(node);
  }

  std::string Visit(ast::Access const *node) final {
    auto qts = context_.qual_types(node);
    if (qts.size() == 1 and
        (qts[0].type().is<type::Enum>() or qts[0].type().is<type::Flags>())) {
      return StringifyExpression(&context_, kind_).Visit(node->operand());
    }
    return absl::StrCat(
        StringifyExpression(&context_, kind_).Visit(node->operand()), ".",
        node->member_name());
  }

  std::string Visit(ast::ArrayLiteral const *node) final {
    // TODO: Look at all the elements type-spellings and choose the best one.
    // TODO: If the element-type is also an array, shorten the lengths, unless
    // the common spelling for elements of that type is a better alias.
    return absl::StrCat("[", node->size(), "; ", Visit(node->elems().front()),
                        "]");
  }

  std::string Visit(ast::ArrayType const *node) final { return "type"; }

  std::string Visit(ast::BinaryOperator const *node) final {
    type::Type operand_type = context_.qual_types(&node->lhs())[0].type();
    if (operand_type.is<type::Primitive>()) { return Visit(&node->lhs()); }
    // TODO: Look at return type of overloaded operators once operator
    // overloading is implemented.
    return context_.qual_types(node)[0].type().to_string();
  }

  std::string Visit(ast::Cast const *node) {
    return StringifyExpression(&context_, kind_).Visit(node->type());
  }

  std::string Visit(ast::Call const *node) final {
    auto const &overload_set = context_.ViableOverloads(node->callee());
    ASSERT(overload_set.members().size() == 1u);
    auto old_kind = std::exchange(kind_, VisitationKind::ReturnType);
    absl::Cleanup c([&] { kind_ = old_kind; });
    return Visit(overload_set.members().front());
  }

  std::string Visit(ast::Declaration const *node) final {
    if (auto const *expr = node->type_expr()) {
      return StringifyExpression(&context_, kind_).Visit(expr);
    } else {
      return StringifyType(&context_, kind_).Visit(node->init_val());
    }
  }

  std::string Visit(ast::Declaration::Id const *node) final {
    // TODO: This may not be the right type.
    return Visit(&node->declaration());
  }

  std::string Visit(ast::DesignatedInitializer const *node) final {
    return StringifyExpression(&context_, kind_).Visit(node->type());
  }

  std::string Visit(ast::FunctionLiteral const *node) final {
    switch (kind_) {
      case VisitationKind::ReturnType: {
        if (node->outputs() and not node->outputs()->empty()) {
          auto old_kind = std::exchange(kind_, VisitationKind::Type);
          absl::Cleanup c([&] { kind_ = old_kind; });
          // TODO: This may not be the right output.
          return StringifyExpression(&context_, kind_)
              .Visit((*node->outputs())[0]);
        } else {
          NOT_YET();
        }
      } break;
      default: NOT_YET();
    }
  }

  std::string Visit(ast::Identifier const *node) final {
    absl::Span<ast::Declaration::Id const *const> decl_ids = context_.decls(node);
    ASSERT(decl_ids.size() == 1u);
    return Visit(decl_ids[0]);
  }

  std::string Visit(ast::Import const *node) final { return "module"; }

  std::string Visit(ast::PatternMatch const *node) final {
    return Visit(&node->pattern());
  }

  std::string Visit(ast::Terminal const *node) final {
    return TerminalType(*node).to_string();
  }

  std::string Visit(ast::UnaryOperator const *node) final {
    switch (node->kind()) {
      case ast::UnaryOperator::Kind::Init:
      case ast::UnaryOperator::Kind::Copy:
      case ast::UnaryOperator::Kind::Move: return Visit(node->operand());
      case ast::UnaryOperator::Kind::Destroy: UNREACHABLE();
      case ast::UnaryOperator::Kind::BufferPointer: return "type";
      case ast::UnaryOperator::Kind::TypeOf: return "type";
      case ast::UnaryOperator::Kind::At:
        return absl::StrCat("@", Visit(node->operand()));
      case ast::UnaryOperator::Kind::Pointer: return "type";
      case ast::UnaryOperator::Kind::Address:
        return absl::StrCat("*", Visit(node->operand()));
      case ast::UnaryOperator::Kind::Negate:
      case ast::UnaryOperator::Kind::Not: {
        type::Type operand_type =
            context_.qual_types(node->operand())[0].type();

        if (operand_type.is<type::Primitive>()) {
          return Visit(node->operand());
        }
        // TODO: Look at return type of overloaded operators once operator
        // overloading is implemented.
        return context_.qual_types(node)[0].type().to_string();
      } break;
    }
  }

 private:
  Context const &context_;
  VisitationKind kind_ = VisitationKind::Type;
};

std::string StringifyExpression::Visit(ast::UnaryOperator const *node) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::BufferPointer:
      return absl::StrCat("[*]", Visit(node->operand()));
    case ast::UnaryOperator::Kind::TypeOf:
      return StringifyType(&context_, kind_).Visit(node->operand());
    case ast::UnaryOperator::Kind::Pointer:
      return absl::StrCat("*", Visit(node->operand()));
    default: NOT_YET();
  }
}

}  // namespace

std::string TypeForDiagnostic(ast::Expression const *expr,
                              Context const &context) {
  return StringifyType(&context, VisitationKind::Type).Visit(expr);
}

std::string ExpressionForDiagnostic(ast::Expression const *expr,
                                    Context const &context) {
  return StringifyExpression(&context, VisitationKind::Type).Visit(expr);
}

}  // namespace compiler
