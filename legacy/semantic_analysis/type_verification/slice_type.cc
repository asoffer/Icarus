#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct SliceDataTypeNotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "slice-data-type-not-a-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Slice type has underlying data type specified as a value which "
            "is not a type."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

}  // namespace

// Verifies that the array type has constant integer lengths and that the data
// type expression is a type.
VerificationTask TypeVerifier::VerifyType(ast::SliceType const *node) {
  std::span data_qts = co_await VerifyTypeOf(&node->data_type());

  bool error    = false;
  bool constant = true;

  if (data_qts.size() != 1) { NTH_UNIMPLEMENTED(); }
  if (data_qts[0].type() != Type) {
    ConsumeDiagnostic(SliceDataTypeNotAType{
        .view = node->data_type().range(),
    });
    error = true;
  } else if (not(data_qts[0].qualifiers() >= Qualifiers::Constant())) {
    constant = false;
  }

  co_return TypeOf(
      node, QualifiedType(
                Type, ((error ? Qualifiers::Error() : Qualifiers()) |
                       (constant ? Qualifiers::Constant() : Qualifiers()))));
}

}  // namespace semantic_analysis
