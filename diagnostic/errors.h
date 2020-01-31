#ifndef ICARUS_DIAGNOSTIC_ERRORS_H
#define ICARUS_DIAGNOSTIC_ERRORS_H

#include <string_view>
#include <vector>

#include "diagnostic/message.h"
#include "frontend/lex/lex.h"
#include "frontend/lex/numbers.h"
#include "frontend/source/range.h"
#include "type/qual_type.h"

namespace diagnostic {

struct ArithmeticBinaryOperatorTypeMismatch {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName =
      "arithmetic-binary-operator-type-mismatch";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        Text("Mismatched types `%s` and `%s` in binary operator.",
             lhs_type->to_string(), rhs_type->to_string())
        /*SourceQuote{}.Highlighted(range, Style{})*/);
  }

  type::Type const* lhs_type;
  type::Type const* rhs_type;
  frontend::SourceRange range;
};

struct NonConstantTypeMemberAccess {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "non-constant-type-member-access";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        Text("Cannot access a member of a non-constant type."));
  }

  frontend::SourceRange range;
};

struct InvalidCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-cast";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(Text("No viable cast form `%s` to `%s`.",
                                  from->to_string(), to->to_string()));
  }

  type::Type const* from;
  type::Type const* to;
  frontend::SourceRange range;
};

struct ImmovableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "immovable-type";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        Text("Attempting to move an immovable type `%s`.", from->to_string()));
  }

  type::Type const* from;
  frontend::SourceRange range;
};

struct MismatchedAssignmentCount {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "mismatched-assignment-count";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        Text("Assigning multiple values but left-hand and right-hand side have "
             "different numbers of elements (`%d` vs. `%d`).",
             to, from));
  }

  size_t to;
  size_t from;
  frontend::SourceRange range;
};

struct MismatchedInitializationCount {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "mismatched-initialization-count";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        Text("Initializing multiple values but left-hand and right-hand side "
             "have different numbers of elements (`%d` vs. `%d`).",
             to, from));
  }

  size_t to;
  size_t from;
  frontend::SourceRange range;
};

struct AssigningToConstant {
  // TODO I'm not sure this shouldn't be in the type-error category.
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "assigning-to-constant";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        Text("It is not allowed to assign to a constant expression. In this "
             "case, the left-hand side of the assignment has type `%s`",
             to->to_string()));
  }

  type::Type const* to;
  frontend::SourceRange range;
};

struct NumberParsingFailure {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "number-parsing-failure";

  DiagnosticMessage ToMessage() const {
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

    return DiagnosticMessage(Text("%s", message));
  }

  frontend::NumberParsingError error;
  frontend::SourceRange range;
};

struct UnprintableSourceCharacter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "unprintable-source-character";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(Text(
        "Uncountered unprintable character with integral value '%d' encountered in source.",
        value));
  }

  int value;
  frontend::SourceRange range;
};

struct InvalidSourceCharacter {
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "invalid-source-character";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        Text("Invalid character '%c' encountered in source.", value));
  }

  char value;
  frontend::SourceRange range;
};


struct StringLiteralParsingFailure{
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "string-literal-parsing-failure";

  DiagnosticMessage ToMessage() const { 
    // TODO: Implement
    return DiagnosticMessage();
  }

  std::vector<frontend::StringLiteralError> errors;
  frontend::SourceRange range;
};

struct HashtagParsingFailure{
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "hashtag-parsing-failure";

  DiagnosticMessage ToMessage() const { 
    // TODO: Implement
    return DiagnosticMessage();
  }

  frontend::SourceRange range;
};

struct NonWhitespaceAfterNewlineEscape{
  static constexpr std::string_view kCategory = "lex";
  static constexpr std::string_view kName     = "non-whitespace-after-newline-escape";

  DiagnosticMessage ToMessage() const { 
    // TODO: Implement
    return DiagnosticMessage();
  }

  frontend::SourceRange range;
};

struct CommaSeparatedListStatement {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "comma-separated-list-statement";

  DiagnosticMessage ToMessage() const { 
    return DiagnosticMessage(
        Text("Comma-separated lists are not allowed as statements"));
  }

  frontend::SourceRange range;
};

struct DeclarationUsedInUnaryOperator {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "declaration-used-in-unary-operator";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        Text("Declarations cannot be used as argument to unary operator."));
  }

  frontend::SourceRange range;
};

struct PositionalArgumentFollowingNamed {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName =
      "positional-argument-followed-by-named";

  DiagnosticMessage ToMessage() const {
    // diagnostic::SourceQuote quote(src_);
    // quote.Highlighted(named_range, diagnostic::Style{});
    // for (auto const &pos_range : pos_ranges) {
    //   quote.Highlighted(pos_range, diagnostic::Style{});
    // }
    return DiagnosticMessage(diagnostic::Text(
        "Positional function arguments cannot follow a named argument."));
  }

  std::vector<frontend::SourceRange> pos_ranges;
  frontend::SourceRange last_named;
};

struct AccessRhsNotIdentifier {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName =
      "access-rhs-not-identifier";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        diagnostic::Text("Right-hand side must be an identifier"));
  }

  frontend::SourceRange range;
};

struct ReservedKeyword {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "reserved-keyword";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        diagnostic::Text("Identifier `%s` is a reserved keyword.", keyword));
  }

  frontend::SourceRange range;
  std::string keyword;
};

struct CallingDeclaration {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "calling-declaration";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text("Declarations cannot be called"));
  }

  frontend::SourceRange range;
};

struct IndexingDeclaration {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "indexing-declaration";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text("Declarations cannot be indexed"));
  }

  frontend::SourceRange range;
};

struct NonDeclarationInStruct {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "non-declaration-in-struct";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text(
        "Each struct member must be defined using a declaration."));
  }

  frontend::SourceRange range;
};

struct UnknownParseError {
  static constexpr std::string_view kCategory = "parse-error";
  static constexpr std::string_view kName     = "unknown-parse-error";

  DiagnosticMessage ToMessage() const {
    // TODO
    // diagnostic::SourceQuote quote(src_);
    // for (auto const& range : lines) {
    //   quote.Highlighted(range, diagnostic::Style{});
    // }
    return DiagnosticMessage(diagnostic::Text(
        "Parse errors found in \"<SOME FILE>\" on the following lines:"));
  }

  std::vector<frontend::SourceRange> lines;
};

struct NoReturnTypes {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-return-type";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text(
        "Attempting to return a value when function returns nothing."));
  }

  frontend::SourceRange range;
};

struct TypeHasNoMembers {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "type-has-no-members";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        diagnostic::Text("Cannot access a member of `type`."));
  }

  frontend::SourceRange range;
};

struct NonConstantModuleMemberAccess {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-constant-module-member-access";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        diagnostic::Text("Cannot access a member of a non-constant module."));
  }

  frontend::SourceRange range;
};

struct NoExportedSymbol {
  static constexpr std::string_view kCategory = "build-error";
  static constexpr std::string_view kName     = "no-exported-symbol";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        diagnostic::Text("No exported symbol of given name in this module."));
  }

  frontend::SourceRange range;
};

struct InconsistentArrayType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "inconsistent-array-type";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text(
        "Type error: Array literal must have consistent type"));
  }

  frontend::SourceRange range;
};

struct NonIntegralArrayLength {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-integral-array-length";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        diagnostic::Text("Array length indexed by non-integral type"));
  }

  frontend::SourceRange range;
};

struct ArrayDataTypeNotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "array-data-type-not-a-type";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(
        diagnostic::Text("Array type has underlying data type specified as a "
                         "value which is not a type."));
  }

  frontend::SourceRange range;
};

struct XorEqNeedsBoolOrFlags {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "xor-needs-bool-or-flags";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text(
        "Operator '^=' must take boolean or flags arguments."));
  }

  frontend::SourceRange range;
};

struct OrEqNeedsBoolOrFlags {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "or-needs-bool-or-flags";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text(
        "Operator '|=' must take boolean or flags arguments."));
  }

  frontend::SourceRange range;
};

struct AndEqNeedsBoolOrFlags {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "and-needs-bool-or-flags";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text(
        "Operator '&=' must take boolean or flags arguments."));
  }

  frontend::SourceRange range;
};

struct NonTypeFunctionInput {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-type-function-output";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text(
        "The specified input type for a function must be a type."));
  }
  frontend::SourceRange range;
};

struct NonTypeFunctionOutput {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-type-function-output";

  DiagnosticMessage ToMessage() const {
    return DiagnosticMessage(diagnostic::Text(
        "The specified return type for a function must be a type."));
  }
  frontend::SourceRange range;
};

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_ERRORS_H
