#include <utility>
#include <vector>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"
#include "core/dependency_node.h"
#include "diagnostic/message.h"

namespace compiler {

struct NotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "not-a-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Expression was expected to be a type, but instead "
                         "was of type `%s`.",
                         type.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
  type::Type type;
};

struct InvalidCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-cast";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No viable cast from `%s` to `%s`.", from.to_string(),
                         to.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type from;
  type::Type to;
  frontend::SourceRange range;
};

struct AssigningToConstant {
  // TODO I'm not sure this shouldn't be in the type-error category.
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "assigning-to-constant";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "It is not allowed to assign to a constant expression. In this "
            "case, the left-hand side of the assignment has type `%s`",
            to.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type to;
  frontend::SourceRange range;
};

struct ImmovableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "immovable-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to move an immovable type `%s`.",
                         from.to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type from;
  frontend::SourceRange range;
};

}  // namespace compiler
