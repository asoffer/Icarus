#include <string_view>

#include "ast/ast.h"
#include "ast/scope.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct ProgramArgumentAccess {
  static constexpr std::string_view kCategory = "access-error";
  static constexpr std::string_view kName     = "program-arguments-access";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Program arguments cannot be accessed from within a function or "
            "user-defined scope. If you would like to access them, they must "
            "be passed in through a parameter."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::ProgramArguments const *node) {
  QualifiedType qualified_type(SliceType(tv.type_system(), Char));
  for (ast::Scope const &s : node->scope()->ancestors()) {
    if (s.kind() == ast::Scope::Kind::BoundaryExecutable and s.parent()) {
      tv.ConsumeDiagnostic(ProgramArgumentAccess{.view = node->range()});
      qualified_type = Error(qualified_type);
      break;
    }
  }

  co_return tv.TypeOf(node, qualified_type);
}

}  // namespace semantic_analysis

