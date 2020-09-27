#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/verify/common.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct CastToNonConstantType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "cast-to-non-constant-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Cannot cast to a type which is not declared constant."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

}  // namespace

type::QualType Compiler::VerifyType(ast::Cast const *node) {
  auto expr_qt = VerifyType(node->expr());
  auto type_qt = VerifyType(node->type());
  if (not expr_qt.ok() or not type_qt.ok()) { return type::QualType::Error(); }

  if (type_qt.type() != type::Type_) {
    diag().Consume(NotAType{.range = node->range(), .type = type_qt.type()});
    return type::QualType::Error();
  }
  if (not type_qt.constant()) {
    diag().Consume(CastToNonConstantType{.range = node->range()});
    return type::QualType::Error();
  }

  auto maybe_type = EvaluateAs<type::Type const *>(node->type());
  if (not maybe_type) {
    diag().Consume(diagnostic::EvaluationFailure{
        .failure = maybe_type.error(),
        .range   = node->range(),
    });
    return type::QualType::Error();
  }
  auto const *t = *maybe_type;
  if (not type::CanCast(expr_qt.type(), t)) {
    diag().Consume(InvalidCast{
        .from  = expr_qt.type(),
        .to    = t,
        .range = node->range(),
    });
  }

  return data().set_qual_type(
      node, type::QualType(t, expr_qt.quals() & ~type::Quals::Buf()));
}

}  // namespace compiler
