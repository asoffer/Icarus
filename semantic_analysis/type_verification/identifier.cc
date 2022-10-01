#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct UncapturedIdentifier {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncaptured-identifier";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Found an identifier '%s' which is not visible in the "
                         "current scope:",
                         id),
        diagnostic::SourceQuote().Highlighted(view,
                                              diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  std::string_view view;
};

[[maybe_unused]] std::vector<ast::Declaration::Id const *> DeclarationIds(
    base::PtrUnion<ast::Declaration::Id const,
                   module::Module::SymbolInformation const>
        symbol_reference) {
  std::vector<ast::Declaration::Id const *> v;
  if (auto const *id = symbol_reference.get_if<ast::Declaration::Id>()) {
    v.push_back(id);
  } else {
    v.push_back(symbol_reference.get<module::Module::SymbolInformation>()->id);
  }
  return v;
}

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Identifier const *node) {
  // TODO: Track cyclic dependencies
  using symbol_ref_type =
      base::PtrUnion<ast::Declaration::Id const,
                     module::Module::SymbolInformation const>;
  std::vector<std::pair<symbol_ref_type, QualifiedType>> viable;

  auto &scope           = *node->scope();
  std::string_view name = node->name();
  for (auto const &id : scope.visible_ancestor_declaration_id_named(name)) {
    absl::Span qts = co_await VerifyTypeOf(&id);
    ASSERT(qts.size() == 1);

    if (qts[0].qualifiers() >= Qualifiers::Error()) {
      co_yield tv.TypeOf(node, Error());
      co_return;
    } else {
      viable.emplace_back(&id, qts[0]);
    }
  }

  // TODO: Get symbols exported from another module.
  // for (ast::Scope const &s : scope.ancestors()) {
  //   for (auto *mod : s.embedded_modules()) {
  //     for (auto const &symbol : mod->Exported(name)) {
  //       viable.emplace_back(&symbol, symbol.qualified_type);
  //     }
  //   }
  // }

  QualifiedType qt;
  switch (viable.size()) {
    case 1: {
      auto const &[symbol_ref, symbol_qt] = viable[0];
      qt                                  = symbol_qt;

      if (qt.qualifiers() >= Qualifiers::Error()) {
        co_yield tv.TypeOf(node, qt);
        co_return;
      }

      if (not(qt.qualifiers() >= Qualifiers::Constant())) {
        qt = Reference(qt);
      }

      co_yield tv.TypeOf(node, qt);
    } break;
    case 0: {
      if (std::empty(
              node->scope()->ancestor_declaration_id_named(node->name()))) {
        tv.ConsumeDiagnostic(compiler::UndeclaredIdentifier{
            .id   = node->name(),
            .view = node->range(),
        });
      } else {
        tv.ConsumeDiagnostic(UncapturedIdentifier{
            .id   = node->name(),
            .view = node->range(),
        });
      }
      qt = Error();
      co_yield tv.TypeOf(node, qt);
    } break;
    default: {
      Qualifiers qualifiers = Qualifiers::Constant();
      absl::flat_hash_set<core::Type> member_types;
      bool error = false;

      for (auto const &[symbol_ref, id_qt] : viable) {
        qt = id_qt;
        if (qt.qualifiers() >= Qualifiers::Error()) { error = true; }
        qualifiers &= qt.qualifiers();
        member_types.insert(qt.type());
      }

      if (error) {
        co_yield tv.TypeOf(node, qt);
        co_return;
      }

      NOT_YET();
      co_yield tv.TypeOf(node, qt);
    } break;
  }
}

}  // namespace semantic_analysis
