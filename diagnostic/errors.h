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

}  // namespace diagnostic

#endif  // ICARUS_DIAGNOSTIC_ERRORS_H
