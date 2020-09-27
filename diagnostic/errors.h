#ifndef ICARUS_DIAGNOSTIC_ERRORS_H
#define ICARUS_DIAGNOSTIC_ERRORS_H

#include <experimental/source_location>
#include <string_view>
#include <vector>

#include "absl/strings/str_cat.h"
#include "core/fn_args.h"
#include "diagnostic/message.h"
#include "frontend/lex/lex.h"
#include "frontend/lex/numbers.h"
#include "frontend/source/file_name.h"
#include "frontend/source/range.h"
#include "frontend/source/source.h"
#include "ir/interpretter/evaluation_failure.h"
#include "type/qual_type.h"
#include "type/tuple.h"

namespace diagnostic {

struct NotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "not-a-type";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text("Expression was expected to be a type, but "
                                  "instead was of type `%s`.",
                                  type->to_string()),
                             SourceQuote(src).Highlighted(range, Style{}));
  }

  frontend::SourceRange range;
  type::Type const *type;
};

struct UnspecifiedOverload {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "unspecified-overload";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Attempting to access an overloaded function by name."),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  frontend::SourceRange range;
};

struct MissingModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "missing-module";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text(
        "Could not find module named \"%s\" requested from %s", source.name(),
        requestor.empty() ? "command line"
                          : absl::StrCat("\"", requestor, "\".")));
  }

  frontend::CanonicalFileName source;
  std::string requestor;
};

struct ParametersDoNotCoverArguments {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "parameters-do-not-cover-arguments";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    // TODO
    return DiagnosticMessage(Text("Parameters do not cover arguments."));
  }

  core::FnArgs<type::QualType> const &args;
};

struct EvaluationFailure {
  static constexpr std::string_view kCategory = "interpretter";
  static constexpr std::string_view kName     = "evaluation-failure";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Compile-time interpretter failed to evaluate expression."),
        SourceQuote(src).Highlighted(range, Style::ErrorText()));
  }

  interpretter::EvaluationFailure failure;
  frontend::SourceRange range;
};

struct Todo {
  static constexpr std::string_view kCategory = "todo";
  static constexpr std::string_view kName     = "todo";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text("TODO: Diagnostic emit from %s, line %u.",
                                  loc.file_name(), loc.line()));
  }

  std::experimental::source_location loc =
      std::experimental::source_location::current();
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_ERRORS_H
