#ifndef ICARUS_IR_INTERPRETER_EVALUATION_RESULT_H
#define ICARUS_IR_INTERPRETER_EVALUATION_RESULT_H

#include <variant>

#include "diagnostic/message.h"
#include "frontend/source/buffer.h"
#include "ir/value/result_buffer.h"

namespace interpreter {

struct EvaluationResult {
  struct Failure {
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

  EvaluationResult(ir::CompleteResultBuffer b) : data_(b) {}
  EvaluationResult(Failure &&f) : data_(std::move(f)) {}
  EvaluationResult(Failure const &f) : data_(f) {}

  Failure error() const { return std::get<Failure>(data_); }

  operator bool() const {
    return std::holds_alternative<ir::CompleteResultBuffer>(data_);
  }

  ir::CompleteResultBuffer operator*() {
    return std::get<ir::CompleteResultBuffer>(data_);
  }
  ir::CompleteResultBuffer *operator->() {
    return &std::get<ir::CompleteResultBuffer>(data_);
  }
  ir::CompleteResultBuffer const *operator->() const {
    return &std::get<ir::CompleteResultBuffer>(data_);
  }

 private:
  std::variant<Failure, ir::CompleteResultBuffer> data_;
};

}  // namespace interpreter

#endif  // ICARUS_IR_INTERPRETER_EVALUATION_RESULT_H
