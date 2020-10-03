#ifndef ICARUS_IR_INTERPRETTER_EVALUATION_FAILURE_H
#define ICARUS_IR_INTERPRETTER_EVALUATION_FAILURE_H

#include "diagnostic/message.h"
#include "frontend/source/range.h"

namespace interpretter {

struct EvaluationFailure {
  static constexpr std::string_view kCategory = "interpretter";
  static constexpr std::string_view kName     = "evaluation-failure";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Compile-time interpretter failed to evaluate expression."),
        diagnostic::SourceQuote(src).Highlighted(
            range, diagnostic::Style::ErrorText()));
  }

  enum class Reason {
    NonConstant,
    Timeout,
    Unknown,
  } failure;
  frontend::SourceRange range;
};

}  // namespace interpretter

#endif  // ICARUS_IR_INTERPRETTER_EVALUATION_FAILURE_H
