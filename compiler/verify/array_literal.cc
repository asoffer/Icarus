#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/verify/verify.h"
#include "type/array.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct InconsistentArrayType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "inconsistent-array-element-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    auto quote = diagnostic::SourceQuote(buffer);
    for (auto const &range : highlights) {
      quote.Highlighted(range, diagnostic::Style{});
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Type error: Array literal must have consistent type"),
        quote);
  }

  frontend::SourceBuffer const * buffer;
  std::vector<frontend::SourceRange> highlights;
};

// Guesses the intended array literal type. For instance, if all but one element
// have the same type, that is probably the intended type. Returns null if it
// cannot determine a reasonable guess.
type::Type GuessIntendedArrayType(
    absl::flat_hash_map<type::Type, int> const &histogram) {
  int total = 0;
  for (auto const &[t, n] : histogram) { total += n; }
  for (auto const &[t, n] : histogram) {
    if (2 * n > total) { return t; }
  }
  return nullptr;
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(ast::ArrayLiteral const *node) {
  if (node->empty()) {
    return context().set_qual_type(node,
                                   type::QualType::Constant(type::EmptyArray));
  }

  std::vector<type::QualType> elem_qts;
  elem_qts.reserve(node->elems().size());
  bool error = false;
  for (auto const *elem : node->elems()) {
    elem_qts.push_back(VerifyType(elem)[0]);
    error &= not elem_qts.back().ok();
  }

  if (error) { return context().set_qual_type(node, type::QualType::Error()); }

  absl::flat_hash_map<type::Type, int> elem_type_count;

  type::Quals quals   = type::Quals::All();
  size_t num_elements = elem_qts.size();
  for (type::QualType const &qt : elem_qts) {
    ++elem_type_count[qt.type()];
    quals &= qt.quals();
  }

  if (elem_type_count.size() == 1) {
    type::Type t = elem_type_count.begin()->first;
    auto qt      = type::QualType(type::Arr(num_elements, t), quals);
    return context().set_qual_type(node, qt);
  } else {
    if (type::Type t = GuessIntendedArrayType(elem_type_count)) {
      std::vector<frontend::SourceRange> mistyped_elements;
      size_t i = 0;
      for (type::QualType const &qt : elem_qts) {
        if (qt.type() != t) {
          mistyped_elements.push_back(node->elems()[i]->range());
        }
        ++i;
      }
      diag().Consume(InconsistentArrayType{
          .buffer     = SourceBufferFor(node),
          .highlights = std::move(mistyped_elements),
      });
      auto qt = type::QualType(type::Arr(num_elements, t), quals);
      qt.MarkError();
      return context().set_qual_type(node, qt);
    } else {
      diag().Consume(InconsistentArrayType{
          .buffer     = SourceBufferFor(node),
          .highlights = {node->range()},
      });
      return context().set_qual_type(node, type::QualType::Error());
    }
  }
}

}  // namespace compiler
