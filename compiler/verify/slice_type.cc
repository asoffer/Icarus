#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/slice.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct SliceDataTypeNotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "slice-data-type-not-a-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Slice type has underlying data type specified as a value which "
            "is not a type."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct NonTypeSliceTypeMatch {
  static constexpr std::string_view kCategory = "pattern-error";
  static constexpr std::string_view kName     = "non-type-slice-type-match";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to match a slice type against a value of type `%s`.",
            type),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Type type;
};


}  // namespace

// Verifies that the slice data type expression is a type.
absl::Span<type::QualType const> Compiler::VerifyType(ast::SliceType const *node) {
  auto quals = type::Quals::Const();

  auto data_qual_type = VerifyType(node->data_type())[0];
  quals &= data_qual_type.quals();
  type::QualType qt(type::Type_, quals);
  if (data_qual_type.type() != type::Type_) {
    diag().Consume(SliceDataTypeNotAType{
        .range = node->data_type()->range(),
    });
    qt.MarkError();
  }

  return context().set_qual_type(node, qt);
}

bool Compiler::VerifyPatternType(ast::SliceType const *node, type::Type t) {
  if (t != type::Type_) {
    diag().Consume(NonTypeSliceTypeMatch{.range = node->range(), .type = t});
    return false;
  }

  return VerifyPatternType(node->data_type(), type::Type_);
}

}  // namespace compiler
