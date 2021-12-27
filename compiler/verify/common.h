#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"
#include "compiler/module.h"
#include "core/dependency_node.h"
#include "diagnostic/message.h"

namespace compiler {

struct InvalidCast {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-cast";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No viable cast from `%s` to `%s`.", from, to),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  std::string from;
  std::string to;
  frontend::SourceView view;
};

struct AssigningToConstant {
  // TODO I'm not sure this shouldn't be in the type-error category.
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "assigning-to-constant";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot assign to a constant (of type `%s`).", to),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  type::Type to;
  frontend::SourceView view;
};

struct ImmovableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "immovable-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Attempting to move an immovable type `%s`.", from),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  type::Type from;
  frontend::SourceView view;
};

struct PatternTypeMismatch {
  static constexpr std::string_view kCategory = "pattern-error";
  static constexpr std::string_view kName     = "pattern-type-mismatch";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            R"(Mismatched type between pattern and expression being matched.
  Type from pattern:          %s
  Type being matched against: %s)",
            pattern_type, matched_type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  type::Type pattern_type;
  std::string matched_type;
  frontend::SourceView view;
};

std::vector<core::Arguments<type::QualType>> YieldArgumentTypes(
    Context const &context,
    base::PtrUnion<ast::BlockNode const, ast::ScopeNode const> node);

type::QualType VerifyCallee(Compiler &c, ast::Expression const *callee,
                            absl::flat_hash_set<type::Type> const &adl_types);

struct VerifyCallParameters{
  ast::Expression const *callee;
  core::Arguments<type::Typed<ir::CompleteResultRef>> const arguments;
};

std::variant<
    std::vector<type::QualType>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyCall(Compiler &c, VerifyCallParameters const &vcp);

}  // namespace compiler
