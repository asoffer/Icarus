#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/array.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct NonIntegralArrayLength {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-integral-array-length";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Array length indexed by non-integral type"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct ArrayDataTypeNotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "array-data-type-not-a-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Array type has underlying data type specified as a value which "
            "is not a type."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

}  // namespace

// Verifies that the array type has constant integer lengths and that the data
// type expression is a type.
absl::Span<type::QualType const> Compiler::VerifyType(ast::ArrayType const *node) {
  std::vector<type::QualType> length_results;
  length_results.reserve(node->lengths().size());
  auto quals = type::Quals::Const();
  for (auto const &len : node->lengths()) {
    auto result = VerifyType(len)[0];
    quals &= result.quals();
    length_results.push_back(result);
    if (not type::IsIntegral(result.type())) {
      diag().Consume(NonIntegralArrayLength{
          .range = node->range(),
      });
    }
  }

  auto data_qual_type = VerifyType(node->data_type())[0];
  quals &= data_qual_type.quals();
  type::QualType qt(type::Type_, quals);
  if (data_qual_type.type() != type::Type_) {
    diag().Consume(ArrayDataTypeNotAType{
        .range = node->data_type()->range(),
    });
    qt.MarkError();
  }

  return context().set_qual_type(node, qt);
}

}  // namespace compiler
