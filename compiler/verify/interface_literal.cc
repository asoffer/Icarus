#include <string>
#include <utility>

#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/module.h"
#include "compiler/type_for_diagnostic.h"
#include "compiler/verify/verify.h"
#include "type/cast.h"

namespace compiler {
namespace {

struct NonInterfaceLiteralMember {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-interface-member";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("All members of an interface literal must have type "
                         "`interface`, but you provided a member of type %s",
                         type),
        diagnostic::SourceQuote().Highlighted(range,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view range;
  std::string type;
};

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::InterfaceLiteral const *node) {
  auto qt = type::QualType::Constant(type::Interface);

  VerifyType(&node->context());

  for (auto const &[name, intf] : node->members()) {
    auto intf_qts = VerifyType(intf.get());
    if (not intf_qts[0].ok()) {
      qt.MarkError();
    } else if (type::CanCastImplicitly(intf_qts[0].type(), type::Interface)) {
      continue;
    } else {
      diag().Consume(NonInterfaceLiteralMember{
          .range = intf->range(),
          .type  = TypeForDiagnostic(intf.get(), context()),
      });
      qt.MarkError();
    }
  }
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
