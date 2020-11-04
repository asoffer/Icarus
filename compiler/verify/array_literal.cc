#include "ast/ast.h"
#include "compiler/compiler.h"
#include "type/array.h"
#include "type/primitive.h"
#include "type/qual_type.h"

namespace compiler {
namespace {

struct InconsistentArrayType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "inconsistent-array-element-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Type error: Array literal must have consistent type"),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

// Guesses the intended array literal type. For instance, if all but one element
// have the same type, that is probably the intended type. Returns null if it
// cannot determine a reasonable guess.
//
// TODO: Improve implementation
type::Type GuessIntendedArrayType(absl::flat_hash_map<type::Type, int>) {
  return nullptr;
}

}  // namespace

type::QualType Compiler::VerifyType(ast::ArrayLiteral const *node) {
  if (node->empty()) {
    return context().set_qual_type(node,
                                   type::QualType::Constant(type::EmptyArray));
  }

  std::vector<type::QualType> elem_qts;
  elem_qts.reserve(node->elems().size());
  bool error = false;
  for (auto const *elem : node->elems()) {
    elem_qts.push_back(VerifyType(elem));
    error &= not elem_qts.back().ok();
  }

  if (error) { return type::QualType::Error(); }

  absl::flat_hash_map<type::Type, int> elem_type_count;

  type::Quals quals   = type::Quals::All();
  size_t num_elements = 0;
  for (type::QualType qt : elem_qts) {
    num_elements += qt.expansion_size();
    qt.ForEach([&](type::Type t) { ++elem_type_count[t]; });
    quals &= qt.quals();
  }

  if (elem_type_count.size() == 1) {
    type::Type t = elem_type_count.begin()->first;
    auto qt      = type::QualType(type::Arr(num_elements, t), quals);
    return context().set_qual_type(node, qt);
  } else {
    diag().Consume(InconsistentArrayType{.range = node->range()});
    if (type::Type t = GuessIntendedArrayType(elem_type_count)) {
      auto qt = type::QualType(type::Arr(num_elements, t), quals);
      qt.MarkError();
      return context().set_qual_type(node, qt);
    } else {
      return type::QualType::Error();
    }
  }
}

}  // namespace compiler
