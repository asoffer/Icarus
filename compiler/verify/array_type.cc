#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/compiler.h"
#include "type/array.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct NonIntegralArrayLength {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-integral-array-length";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Array length indexed by non-integral type"),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
};

struct ArrayDataTypeNotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "array-data-type-not-a-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Array type has underlying data type specified as a value which "
            "is not a type."),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
};

struct NonTypeArrayTypeMatch {
  static constexpr std::string_view kCategory = "pattern-error";
  static constexpr std::string_view kName     = "non-type-array-type-match";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to match an array type against a value of type `%s`.",
            type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
  type::Type type;
};

}  // namespace

// Verifies that the array type has constant integer lengths and that the data
// type expression is a type.
absl::Span<type::QualType const> Compiler::VerifyType(
    ast::ArrayType const *node) {
  std::vector<type::QualType> length_results;
  length_results.reserve(node->lengths().size());
  auto quals = type::Quals::Const();
  for (auto const &len : node->lengths()) {
    auto result = VerifyType(len)[0];
    quals &= result.quals();
    length_results.push_back(result);
    if (not type::IsIntegral(result.type())) {
      diag().Consume(NonIntegralArrayLength{
          .view = SourceViewFor(node),
      });
    }
  }

  auto data_qual_type = VerifyType(node->data_type())[0];
  quals &= data_qual_type.quals();
  type::QualType qt(type::Type_, quals);
  if (data_qual_type.type() != type::Type_) {
    diag().Consume(ArrayDataTypeNotAType{
        .view = SourceViewFor(node->data_type()),
    });
    qt.MarkError();
  }

  return context().set_qual_type(node, qt);
}

bool Compiler::VerifyPatternType(ast::ArrayType const *node, type::Type t) {
  if (t != type::Type_) {
    diag().Consume(
        NonTypeArrayTypeMatch{.view = SourceViewFor(node), .type = t});
    return false;
  }

  EnqueueVerifyPatternMatchType(node->data_type(), type::Type_);
  for (auto const *expr : node->lengths()) {
    EnqueueVerifyPatternMatchType(expr, type::Integer);
  }

  return true;
}

}  // namespace compiler
