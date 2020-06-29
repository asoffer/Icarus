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

struct InvalidCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-cast";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text("No viable cast from `%s` to `%s`.",
                                  from->to_string(), to->to_string()),
                             SourceQuote(src).Highlighted(range, Style{}));
  }

  type::Type const *from;
  type::Type const *to;
  frontend::SourceRange range;
};

struct ImmovableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "immovable-type";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Attempting to move an immovable type `%s`.", from->to_string()),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  type::Type const *from;
  frontend::SourceRange range;
};

struct UncopyableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncopyable-type";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Attempting to copy an uncopyable type `%s`.", from->to_string()),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  type::Type const *from;
  frontend::SourceRange range;
};


struct AssigningToConstant {
  // TODO I'm not sure this shouldn't be in the type-error category.
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "assigning-to-constant";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("It is not allowed to assign to a constant expression. In this "
             "case, the left-hand side of the assignment has type `%s`",
             to->to_string()),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  type::Type const *to;
  frontend::SourceRange range;
};

struct NoReturnTypes {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-return-type";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Attempting to return a value when function returns nothing."),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  frontend::SourceRange range;
};

struct ReturnTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "return-type-mismatch";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Returning an expression of type `%s` from a function which "
             "returns `%s`.",
             actual->to_string(), expected->to_string()),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  type::Type const *actual;
  type::Type const *expected;
  frontend::SourceRange range;
};

struct ReturningWrongNumber {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "returning-wrong-number";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Attempting to return %u values from a function which has %u "
             "return values.",
             actual, expected),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  size_t actual;
  size_t expected;
  frontend::SourceRange range;
};

struct IndexedReturnTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "indexed-return-type-mismatch";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Returning an expression in slot #%u (zero-indexed) of type `%s` "
             "but "
             "function expects a value of type `%s` in that slot.",
             index, actual->to_string(), expected->to_string()),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  size_t index;
  type::Type const *actual;
  type::Type const *expected;
  frontend::SourceRange range;
};

struct CastToNonType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "cast-to-non-type";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text("Cannot cast to a non-type."),
                             SourceQuote(src).Highlighted(range, Style{}));
  }

  frontend::SourceRange range;
};

struct CastToNonConstantType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "cast-to-non-constant-type";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Cannot cast to a type which is not declared constant."),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  frontend::SourceRange range;
};

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

struct InvalidImport {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-import";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text("Cannot import a non-constant module."),
                             SourceQuote(src).Highlighted(range, Style{}));
  }

  frontend::SourceRange range;
};

struct NonConstantImport {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-import";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text("Scope names must be constant."),
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
