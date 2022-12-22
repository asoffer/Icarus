#include "ast/ast.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct InconsistentArrayType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "inconsistent-array-element-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    diagnostic::SourceQuote quote;
    for (auto const &range : highlights) {
      quote.Highlighted(range, diagnostic::Style{});
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Type error: Array literal must have consistent type"),
        quote);
  }

  std::vector<std::string_view> highlights;
};

// Guesses the intended array literal type. For instance, if all but one element
// have the same type, that is probably the intended type. Returns null if it
// cannot determine a reasonable guess.
std::optional<core::Type> GuessIntendedArrayType(
    absl::flat_hash_map<core::Type, int> const &histogram) {
  int total = 0;
  for (auto const &[t, n] : histogram) { total += n; }
  for (auto const &[t, n] : histogram) {
    if (2 * n > total) { return t; }
  }
  return std::nullopt;
}

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::ArrayLiteral const *node) {
  if (node->empty()) { co_return tv.TypeOf(node, Constant(EmptyArray)); }

  std::vector<QualifiedType> element_qualified_types;
  element_qualified_types.reserve(node->elements().size());
  bool error = false;
  for (auto const *elem : node->elements()) {
    std::span qts = co_await VerifyTypeOf(elem);
    if (qts.size() != 1) { NOT_YET(); }
    if (qts[0].qualifiers() >= Qualifiers::Error()) {
      error = true;
    } else {
      element_qualified_types.push_back(qts[0]);
    }
  }

  if (error) { co_return tv.TypeOf(node, Error()); }

  absl::flat_hash_map<core::Type, int> element_type_count;

  QualifiedType qt;
  size_t num_elements = element_qualified_types.size();

  Qualifiers qualifiers = Qualifiers::Constant();
  for (QualifiedType qualified_type : element_qualified_types) {
    ++element_type_count[qualified_type.type()];
    qualifiers &= qualified_type.qualifiers();
  }

  if (element_type_count.size() == 1) {
    core::Type t = element_type_count.begin()->first;
    qt =
        QualifiedType(ArrayType(tv.type_system(), num_elements, t), qualifiers);
  } else if (std::optional<core::Type> t =
                 GuessIntendedArrayType(element_type_count)) {
    std::vector<std::string_view> mistyped_elements;
    size_t i = 0;
    for (QualifiedType qt : element_qualified_types) {
      if (qt.type() != *t) {
        mistyped_elements.push_back(node->elements()[i]->range());
      }
      ++i;
    }
    tv.ConsumeDiagnostic(InconsistentArrayType{
        .highlights = std::move(mistyped_elements),
    });

    qt = Error(QualifiedType(ArrayType(tv.type_system(), num_elements, *t),
                             qualifiers));
  } else {
    tv.ConsumeDiagnostic(InconsistentArrayType{
        .highlights = {node->range()},
    });

    qt = Error(qualifiers);
  }

  co_return tv.TypeOf(node, qt);
}

}  // namespace semantic_analysis
