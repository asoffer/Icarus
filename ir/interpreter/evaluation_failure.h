#ifndef ICARUS_IR_INTERPRETER_EVALUATION_FAILURE_H
#define ICARUS_IR_INTERPRETER_EVALUATION_FAILURE_H

#include "diagnostic/message.h"
#include "frontend/source/range.h"

namespace interpreter {

struct EvaluationFailure {
  static constexpr std::string_view kCategory = "interpreter";
  static constexpr std::string_view kName     = "evaluation-failure";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Compile-time interpreter failed to evaluate expression."),
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

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EVALUATION_FAILURE_H
