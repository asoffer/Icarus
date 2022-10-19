#include "semantic_analysis/type_verification/diagnostics.h"

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

  std::string operator()(ast::Access const *node) { NOT_YET(); }

  std::string operator()(ast::ArrayLiteral const *node) { NOT_YET(); }

  std::string operator()(ast::ArrayType const *node) { NOT_YET(); }

  std::string operator()(ast::BinaryOperator const *node) { NOT_YET(); }

  std::string operator()(ast::Cast const *node) { NOT_YET(); }

  std::string operator()(ast::Call const *node) { NOT_YET(); }

  std::string operator()(ast::Declaration::Id const *node) { NOT_YET(); }

  std::string operator()(ast::DesignatedInitializer const *node) { NOT_YET(); }

  std::string operator()(ast::FunctionLiteral const *node) { NOT_YET(); }

  std::string operator()(ast::Identifier const *node) { NOT_YET(); }

  std::string operator()(ast::Import const *node) { NOT_YET(); }

  std::string operator()(ast::Index const *node) { NOT_YET(); }

  std::string operator()(ast::Module const *node) { NOT_YET(); }

  std::string operator()(ast::PatternMatch const *node) { NOT_YET(); }

  std::string operator()(ast::Terminal const *node) { return std::string(node->range()); }

  std::string operator()(ast::UnaryOperator const *node) { NOT_YET(); }

 private:
  Context const &context_;
  TypeSystem &type_system_;
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

  std::string operator()(ast::Access const *node) { NOT_YET(); }

  std::string operator()(ast::ArrayLiteral const *node) {
    return "[TODO; TODO]";
  }

  std::string operator()(ast::ArrayType const *node) { return "type"; }

  std::string operator()(ast::BinaryOperator const *node) { NOT_YET(); }

  std::string operator()(ast::Cast const *node) {
    return ExpressionForDiagnostic(*node->type(), context_, type_system_);
  }

  std::string operator()(ast::Call const *node) { NOT_YET(); }

  std::string operator()(ast::Declaration::Id const *node) {
    if (node->declaration().ids().size() != 1) { NOT_YET(); }
    if (auto *type_expr = node->declaration().type_expr()) {
      return ExpressionForDiagnostic(*type_expr, context_, type_system_);
    } else {
      return operator()(node->declaration().init_val());
    }
  }

  std::string operator()(ast::DesignatedInitializer const *node) { NOT_YET(); }

  std::string operator()(ast::FunctionLiteral const *node) { NOT_YET(); }

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
    } else {
      NOT_YET();
    }
  }

  std::string operator()(ast::UnaryOperator const *node) { NOT_YET(); }

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
