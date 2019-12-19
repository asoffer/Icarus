#ifndef ICARUS_DIAGNOSTIC_ERRORS_H
#define ICARUS_DIAGNOSTIC_ERRORS_H

#include <string_view>

#include "frontend/source/range.h"
#include "type/qual_type.h"

namespace diagnostic {

struct ArithmeticBinaryOperatorTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "arithmetic-binary-operator-type-mismatch";

  DiagnosticMessage ToMessage() {
    return DiagnosticMessage(
        Text("Mismatched types `%s` and `%s` in binary operator.",
             lhs_type->to_string(), rhs_type->to_string())
        /*SourceQuote{}.Highlighted(range, Style{})*/);
  }

  type::Type const* lhs_type;
  type::Type const* rhs_type;
  frontend::SourceRange range;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_ERRORS_H
