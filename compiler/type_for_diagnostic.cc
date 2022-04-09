#include "compiler/type_for_diagnostic.h"

#include <sstream>

#include "absl/cleanup/cleanup.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"
#include "ast/module.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/slice.h"

namespace compiler {
namespace {

enum class VisitationKind { Type, ReturnType };

struct StringifyExpression {
  using signature = std::string();

  explicit StringifyExpression(Context const *c, VisitationKind kind)
      : context_(*ASSERT_NOT_NULL(c)), kind_(kind) {}

  std::string operator()(ast::Node const *node) {
    return ASSERT_NOT_NULL(node)->visit<StringifyExpression>(*this);
  }

  std::string operator()(ast::Access const *node) {
    return absl::StrCat((*this)(node->operand()), ".", node->member_name());
  }

  std::string operator()(ast::ArrayType const *node) {
    return absl::StrCat(
        "[",
        absl::StrJoin(node->lengths(), ", ",
                      [&](std::string *out, auto const *length_expr) {
                        absl::StrAppend(out, operator()(length_expr));
                      }),
        "; ", (*this)(node->data_type()), "]");
  }

  std::string operator()(ast::BindingDeclaration const *node) {
    return std::string(node->ids()[0].name());
  }

  std::string operator()(ast::Call const *node) {
    return absl::StrCat(
        (*this)(node->callee()), "(",
        absl::StrJoin(node->arguments(), ", ",
                      [&](std::string *out, auto const &arg) {
                        if (arg.named()) {
                          absl::StrAppend(out, arg.name(),
                                          " = ", operator()(&arg.expr()));
                        } else {
                          absl::StrAppend(out, operator()(&arg.expr()));
                        }
                      }),
        ")");
  }

  std::string operator()(ast::Identifier const *node) {
    return std::string(node->name());
  }

  std::string operator()(ast::PatternMatch const *node) {
    return operator()(&node->pattern());
  }

  std::string operator()(ast::Terminal const *node) {
    std::stringstream ss;
    context_.qual_types(node)[0].type().ShowValue(ss, node->value());
    return ss.str();
  }

  std::string operator()(ast::UnaryOperator const *node);

 private:
  Context const &context_;
  VisitationKind kind_ = VisitationKind::Type;
};

struct StringifyType {
  using signature = std::string();
  explicit StringifyType(Context const *c, VisitationKind kind)
      : context_(*ASSERT_NOT_NULL(c)), kind_(kind) {}

  std::string operator()(ast::Node const *node) {
    return ASSERT_NOT_NULL(node)->visit<StringifyType>(*this);
  }

  std::string operator()(ast::Access const *node) {
    auto qts = context_.qual_types(node);
    if (qts.size() == 1 and
        (qts[0].type().is<type::Enum>() or qts[0].type().is<type::Flags>())) {
      return StringifyExpression(&context_, kind_)(node->operand());
    } else {
      auto operand_qts = context_.qual_types(node->operand());
      if (operand_qts[0].type().is<type::Slice>()) {
        if (node->member_name() == "length") { return "u64"; }
      }
      // TODO: This is wrong.
      return absl::StrCat(StringifyType(&context_, kind_)(node->operand()), ".",
                          node->member_name());
    }
  }

  std::string operator()(ast::ArrayLiteral const *node) {
    // TODO: Look at all the elements type-spellings and choose the best one.
    // TODO: If the element-type is also an array, shorten the lengths, unless
    // the common spelling for elements of that type is a better alias.
    return absl::StrCat("[", node->size(), "; ", (*this)(node->elems().front()),
                        "]");
  }

  std::string operator()(ast::ArrayType const *node) { return "type"; }

  std::string operator()(ast::BinaryOperator const *node) {
    type::Type lhs_type = context_.qual_types(&node->lhs())[0].type();
    type::Type rhs_type = context_.qual_types(&node->rhs())[0].type();
    if (lhs_type.is<type::Primitive>() and rhs_type.is<type::Primitive>()) {
      return operator()(&node->lhs());
    }
    // TODO: Look at return type of overloaded operators once operator
    // overloading is implemented.
    return context_.qual_types(node)[0].type().to_string();
  }

  std::string operator()(ast::Cast const *node) {
    return StringifyExpression(&context_, kind_)(node->type());
  }

  std::string operator()(ast::Call const *node) {
    auto old_kind = std::exchange(kind_, VisitationKind::ReturnType);
    absl::Cleanup c([&] { kind_ = old_kind; });
    // TODO: Handle the case where it's imported.
    return operator()(
        context_.CallMetadata(node).resolved().get<ast::Expression>());
  }

  std::string operator()(ast::Declaration const *node) {
    if (auto const *expr = node->type_expr()) {
      return StringifyExpression(&context_, kind_)(expr);
    } else {
      return StringifyType(&context_, kind_)(node->init_val());
    }
  }

  std::string operator()(ast::Declaration::Id const *node) {
    // TODO: This may not be the right type.
    return operator()(&node->declaration());
  }

  std::string operator()(ast::DesignatedInitializer const *node) {
    return StringifyExpression(&context_, kind_)(node->type());
  }

  std::string operator()(ast::FunctionLiteral const *node) {
    switch (kind_) {
      case VisitationKind::ReturnType: {
        if (node->outputs() and not node->outputs()->empty()) {
          auto old_kind = std::exchange(kind_, VisitationKind::Type);
          absl::Cleanup c([&] { kind_ = old_kind; });
          // TODO: This may not be the right output.
          return StringifyExpression(&context_, kind_)((*node->outputs())[0]);
        } else {
          NOT_YET();
        }
      } break;
      default: NOT_YET();
    }
  }

  std::string operator()(ast::Identifier const *node) {
    absl::Span<ast::Declaration::Id const *const> decl_ids =
        context_.decls(node);
    ASSERT(decl_ids.size() == 1u);
    return operator()(decl_ids[0]);
  }

  std::string operator()(ast::Import const *node) { return "module"; }

  std::string operator()(ast::Index const *node) {
    // TODO: This is tricky to implement correctly, even if we're looking
    // directly at an array. We want to look at the type as spelled at the
    // declaration site of the array, (if it even has one) and extract the
    // element type from that. But if the type is inferred we want to progress
    // further.
    return context_.qual_types(node)[0].type().to_string();
  }

  std::string operator()(ast::PatternMatch const *node) {
    return operator()(&node->pattern());
  }

  std::string operator()(ast::Terminal const *node) {
    return TerminalType(*node).to_string();
  }

  std::string operator()(ast::UnaryOperator const *node) {
    switch (node->kind()) {
      case ast::UnaryOperator::Kind::Init:
      case ast::UnaryOperator::Kind::Copy:
      case ast::UnaryOperator::Kind::Move: return (*this)(node->operand());
      case ast::UnaryOperator::Kind::Destroy: UNREACHABLE();
      case ast::UnaryOperator::Kind::BufferPointer: return "type";
      case ast::UnaryOperator::Kind::TypeOf: return "type";
      case ast::UnaryOperator::Kind::At:
        return absl::StrCat("@", (*this)(node->operand()));
      case ast::UnaryOperator::Kind::Pointer: return "type";
      case ast::UnaryOperator::Kind::Address:
        return absl::StrCat("*", (*this)(node->operand()));
      case ast::UnaryOperator::Kind::BlockJump:
        return absl::StrCat(">> ", (*this)(node->operand()));
      case ast::UnaryOperator::Kind::Negate:
      case ast::UnaryOperator::Kind::Not: {
        type::Type operand_type =
            context_.qual_types(node->operand())[0].type();

        if (operand_type.is<type::Primitive>()) {
          return (*this)(node->operand());
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

std::string StringifyExpression::operator()(ast::UnaryOperator const *node) {
  switch (node->kind()) {
    case ast::UnaryOperator::Kind::BufferPointer:
      return absl::StrCat("[*]", (*this)(node->operand()));
    case ast::UnaryOperator::Kind::TypeOf:
      return StringifyType(&context_, kind_)(node->operand());
    case ast::UnaryOperator::Kind::Pointer:
      return absl::StrCat("*", (*this)(node->operand()));
    default: NOT_YET();
  }
}

}  // namespace

std::string TypeForDiagnostic(ast::Expression const *expr,
                              Context const &context) {
  return StringifyType(&context, VisitationKind::Type)(expr);
}

std::string ExpressionForDiagnostic(ast::Expression const *expr,
                                    Context const &context) {
  return StringifyExpression(&context, VisitationKind::Type)(expr);
}

}  // namespace compiler
