#include <utility>
#include <vector>

#include "ast/ast.h"
#include "compiler/data.h"
#include "core/dependency_node.h"
#include "diagnostic/message.h"

namespace compiler {

struct InvalidCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-cast";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No viable cast from `%s` to `%s`.", from->to_string(),
                         to->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *from;
  type::Type const *to;
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
            to->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *to;
  frontend::SourceRange range;
};

struct ImmovableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "immovable-type";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to move an immovable type `%s`.",
                         from->to_string()),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type const *from;
  frontend::SourceRange range;
};

// Given a parameterized expression returns a vector consisting of dependency
// nodes for each parameter declaration, and the index of that declaration.
// There are four types of dependency (representing all combinations of the type
// or value of the parameter and of the argument bound to a given parameter).
std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
OrderedDependencyNodes(ast::ParameterizedExpression const *node);

// TODO: Document
DependentComputedData::InsertDependentResult MakeConcrete(
    ast::ParameterizedExpression const *node, CompiledModule *mod,
    absl::Span<std::pair<int, core::DependencyNode<ast::Declaration>> const>
        ordered_nodes,
    core::FnArgs<type::Typed<ir::Value>> const &args,
    DependentComputedData &compiler_data, diagnostic::DiagnosticConsumer &diag);

}  // namespace compiler
