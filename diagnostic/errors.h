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
#include "interpretter/evaluation_failure.h"
#include "type/qual_type.h"
#include "type/tuple.h"

namespace diagnostic {

struct InvalidCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-cast";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(Text("No viable cast form `%s` to `%s`.",
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

struct MismatchedAssignmentCount {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "mismatched-assignment-count";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Assigning multiple values but left-hand and right-hand side have "
             "different numbers of elements (`%d` vs. `%d`).",
             to, from),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  size_t to;
  size_t from;
  frontend::SourceRange range;
};

struct MismatchedInitializationCount {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "mismatched-initialization-count";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Initializing multiple values but left-hand and right-hand side "
             "have different numbers of elements (`%d` vs. `%d`).",
             to, from),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  size_t to;
  size_t from;
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

struct NumberParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "number-parsing-failure";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    std::string_view message;
    switch (error) {
      case frontend::NumberParsingError::kUnknownBase:
        message = "Unknown base for numeric literal";
        break;
      case frontend::NumberParsingError::kTooManyDots:
        message = "Too many `.` characters in numeric literal";
        break;
      case frontend::NumberParsingError::kNoDigits:
        message = "No digits in numeric literal";
        break;
      case frontend::NumberParsingError::kInvalidDigit:
        message = "Invalid digit encountered";
        break;
      case frontend::NumberParsingError::kTooLarge:
        message = "Numeric literal is too large";
        break;
    }

    return DiagnosticMessage(Text("%s", message),
                             SourceQuote(src).Highlighted(range, Style{}));
  }

  frontend::NumberParsingError error;
  frontend::SourceRange range;
};

struct UnprintableSourceCharacter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "unprintable-source-character";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Encountered unprintable character with integral value '%d' "
             "encountered in source.",
             value));
  }

  int value;
  frontend::SourceRange range;
};

struct InvalidSourceCharacter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "invalid-source-character";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Invalid character '%c' encountered in source.", value),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  char value;
  frontend::SourceRange range;
};

struct StringLiteralParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName = "string-literal-parsing-failure";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    // TODO: Implement
    return DiagnosticMessage();
  }

  std::vector<frontend::StringLiteralError> errors;
  frontend::SourceRange range;
};

struct HashtagParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "hashtag-parsing-failure";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    // TODO: Implement
    return DiagnosticMessage();
  }

  frontend::SourceRange range;
};

struct NonWhitespaceAfterNewlineEscape {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName =
      "non-whitespace-after-newline-escape";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    // TODO: Implement
    return DiagnosticMessage();
  }

  frontend::SourceRange range;
};

struct EmptySource {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "empty-source";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Source file is empty or contains only whitespace."));
  }
  // TODO source file identifier?
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

struct OrEqNeedsBoolOrFlags {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "or-needs-bool-or-flags";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Operator '|=' must take boolean or flags arguments."),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  frontend::SourceRange range;
};

struct AndEqNeedsBoolOrFlags {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "and-needs-bool-or-flags";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Operator '&=' must take boolean or flags arguments."),
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

struct SwitchConditionNeedsBool {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "switch-condition-needs-bool";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Expressionless switch conditions must evaluate to a `bool`, but "
             "you provided a `%s`.",
             type->to_string()),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  type::Type const *type;
  frontend::SourceRange range;
};

struct PreconditionNeedsBool {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "pre-condition-needs-bool";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Function precondition must be of type bool, but you provided an "
             "expression of type %s.",
             type->to_string()),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  type::Type const *type;
  frontend::SourceRange range;
};

struct PostconditionNeedsBool {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "postcondition-needs-bool";

  DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return DiagnosticMessage(
        Text("Function postcondition must be of type bool, but you provided an "
             "expression of type %s.",
             type->to_string()),
        SourceQuote(src).Highlighted(range, Style{}));
  }

  type::Type const *type;
  frontend::SourceRange range;
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
