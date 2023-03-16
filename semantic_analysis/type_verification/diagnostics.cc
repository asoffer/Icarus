#include "semantic_analysis/type_verification/diagnostics.h"

#include "absl/cleanup/cleanup.h"
#include "absl/strings/str_cat.h"
#include "absl/strings/str_join.h"
#include "ast/ast.h"

namespace semantic_analysis {
namespace {

enum class VisitationKind { Type, ReturnType };

struct StringifyExpression {
  using signature = std::string();
  explicit StringifyExpression(Context const *c, TypeSystem *ts)
      : context_(*ASSERT_NOT_NULL(c)), type_system_(*ASSERT_NOT_NULL(ts)) {}

  std::string operator()(ast::Node const *node) {
    return ASSERT_NOT_NULL(node)->visit<StringifyExpression>(*this);
  }

  std::string operator()(ast::Access const *node) {
    return absl::StrCat(operator()(node->operand()), ".", node->member_name());
  }

  std::string operator()(ast::ArrayLiteral const *node) { NOT_YET(); }

  std::string operator()(ast::ArrayType const *node) { NOT_YET(); }

  std::string operator()(ast::BinaryOperator const *node) { NOT_YET(); }

  std::string operator()(ast::Cast const *node) { NOT_YET(); }

  std::string operator()(ast::Call const *node) { NOT_YET(); }

  std::string operator()(ast::Declaration::Id const *node) { NOT_YET(); }

  std::string operator()(ast::DesignatedInitializer const *node) { NOT_YET(); }

  std::string operator()(ast::FunctionLiteral const *node) { NOT_YET(); }

  std::string operator()(ast::Identifier const *node) {
    return std::string(node->name());
  }

  std::string operator()(ast::Import const *node) { NOT_YET(); }

  std::string operator()(ast::Index const *node) { NOT_YET(); }

  std::string operator()(ast::Module const *node) { NOT_YET(); }

  std::string operator()(ast::PatternMatch const *node) { NOT_YET(); }

  std::string operator()(ast::Terminal const *node) {
    return std::string(node->range());
  }

  std::string operator()(ast::UnaryOperator const *node) {
    // TODO
    return std::string(node->range());
  }

 private:
  [[maybe_unused]] Context const &context_;
  [[maybe_unused]] TypeSystem &type_system_;
};

struct StringifyType {
  using signature = std::string();
  explicit StringifyType(Context const *c, TypeSystem *ts, VisitationKind kind)
      : context_(*ASSERT_NOT_NULL(c)),
        type_system_(*ASSERT_NOT_NULL(ts)),
        kind_(kind) {}

  std::string operator()(ast::Node const *node) {
    return ASSERT_NOT_NULL(node)->visit<StringifyType>(*this);
  }

  std::string operator()(ast::Access const *node) {
    NOT_YET(node->DebugString());
  }

  std::string operator()(ast::ArrayLiteral const *node) {
    // TODO: We can probably develop a better heuristic for the element type by
    // looking at all entries and picking the most common one.
    //
    // TODO: For nested arrays it will sometimes make sense to write this in the
    // form `[N; T]` where `T` is itself an array-type, and other times it will
    // make sense to write this as `[N, M; T]`.
    return absl::StrCat("[", node->size(),
                        "; ", operator()(node->elements()[0]), "]");
  }

  std::string operator()(ast::ArrayType const *node) { return "type"; }

  std::string operator()(ast::BinaryOperator const *node) { NOT_YET(); }

  std::string operator()(ast::Cast const *node) {
    return ExpressionForDiagnostic(*node->type(), context_, type_system_);
  }

  std::string operator()(ast::Call const *node) {
    auto old_kind = std::exchange(kind_, VisitationKind::ReturnType);
    absl::Cleanup c([&] { kind_ = old_kind; });
    // TODO: Handle the case where it's imported.
    if (auto const *i = node->callee()->if_as<ast::Identifier>()) {
      auto symbol = context_.symbol(i);
      if (auto const *id = symbol.get_if<ast::Declaration::Id>()) {
        return operator()(id);
      } else {
        NOT_YET();
      }
    } else {
      NOT_YET();
    }
  }

  std::string operator()(ast::Declaration::Id const *node) {
    if (node->declaration().ids().size() != 1) { NOT_YET(); }
    if (auto *type_expr = node->declaration().type_expr()) {
      return ExpressionForDiagnostic(*type_expr, context_, type_system_);
    } else {
      return operator()(node->declaration().init_val());
    }
  }

  std::string operator()(ast::DesignatedInitializer const *node) { NOT_YET(); }

  std::string operator()(ast::FunctionLiteral const *node) {
    switch (kind_) {
      case VisitationKind::ReturnType: {
        if (node->outputs()) {
          auto old_kind = std::exchange(kind_, VisitationKind::Type);
          absl::Cleanup c([&] { kind_ = old_kind; });
          std::string comma_separated_types =
              absl::StrJoin(*node->outputs(), ", ",
                            [&](std::string *out, ast::Expression const *expr) {
                              out->append(ExpressionForDiagnostic(
                                  *expr, context_, type_system_));
                            });

          if (node->outputs()->size() == 1) {
            return comma_separated_types;
          } else {
            return absl::StrCat("(", comma_separated_types, ")");
          }
        } else {
          NOT_YET();
        }
      } break;
      case VisitationKind::Type: {
        NOT_YET();
      } break;
    }
  }

  std::string operator()(ast::Identifier const *node) {
    auto symbol = context_.symbol(node);
    return operator()(symbol.get<ast::Declaration::Id>());
  }

  std::string operator()(ast::Import const *node) { return "module"; }

  std::string operator()(ast::Index const *node) { NOT_YET(); }

  std::string operator()(ast::Module const *node) { NOT_YET(); }

  std::string operator()(ast::PatternMatch const *node) { NOT_YET(); }

  std::string operator()(ast::Terminal const *node) {
    core::Type t = context_.qualified_type(node).type();
    if (auto p = t.get_if<PrimitiveType>(type_system_)) {
      static constexpr std::array kPrimitiveTypes{
          "bool", "char",    "byte",   "f32",         "f64",
          "type", "integer", "module", "empty-array", "error"};
      size_t value =
          static_cast<std::underlying_type_t<decltype(p->value())>>(p->value());
      ASSERT(value < kPrimitiveTypes.size());
      return kPrimitiveTypes[value];
    } else if (auto i = t.get_if<core::SizedIntegerType>(type_system_)) {
      std::string result =
          absl::StrCat((i->is_signed() ? "i" : "u"), i->bits());
      if (i->alignment() !=
          core::SizedIntegerType::DefaultAlignment(i->bits())) {
        NOT_YET();
      }
      return result;
    } else if (auto i = t.get_if<SliceType>(type_system_)) {
      return "[/]char";
    } else {
      NOT_YET(node->DebugString());
    }
  }

  std::string operator()(ast::UnaryOperator const *node) {
    // TODO: Many of these require correct handling of precedence.
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
        core::Type t = context_.qualified_type(node).type();
        if (t.is<PrimitiveType>(type_system_) or
            t.is<core::SizedIntegerType>(type_system_)) {
          return (*this)(node->operand());
        } else {
          NOT_YET(node->DebugString(), DebugType(t, type_system_));
        }
      } break;
    }
  }

 private:
  Context const &context_;
  TypeSystem &type_system_;
  [[maybe_unused]] VisitationKind kind_ = VisitationKind::Type;
};

}  // namespace

std::string TypeForDiagnostic(ast::Expression const &expr,
                              Context const &context, TypeSystem &type_system) {
  return StringifyType(&context, &type_system, VisitationKind::Type)(&expr);
}

std::string ExpressionForDiagnostic(ast::Expression const &expr,
                                    Context const &context,
                                    TypeSystem &type_system) {
  return StringifyExpression(&context, &type_system)(&expr);
}

}  // namespace semantic_analysis
