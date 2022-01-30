#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope.h"
#include "compiler/context.h"
#include "compiler/verify/verify.h"
#include "type/primitive.h"
#include "type/qual_type.h"
#include "type/slice.h"

namespace compiler {
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
        diagnostic::SourceQuote()
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
};

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::ProgramArguments const *node) {
  auto qt  = type::QualType::NonConstant(type::Slc(type::Slc(type::Char)));
  for (ast::Scope const &s : node->scope()->ancestors()) {
    if (s.kind() == ast::Scope::Kind::BoundaryExecutable and s.parent()) {
      diag().Consume(ProgramArgumentAccess{.view = SourceViewFor(node)});
      qt.MarkError();
      break;
    }
  }

  return context().set_qual_type(node, qt);
}

}  // namespace compiler
