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

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Identifier const *node) {
  if (node->name() == "builtin") {
    co_return tv.TypeOf(node, Constant(Module));
  }

  // TODO: Track cyclic dependencies
  using symbol_ref_type = Context::symbol_ref_type;
  std::vector<std::pair<symbol_ref_type, QualifiedType>> viable;

  auto &scope           = *node->scope();
  std::string_view name = node->name();

  absl::flat_hash_map<core::ParameterType, Context::CallableIdentifier>
      parameters_options;
  for (auto const &id : scope.visible_ancestor_declaration_id_named(name)) {
    std::span parameters = co_await VerifyParametersOf(&id);
    if (parameters.data() == nullptr) { continue; }

    ASSERT(parameters.size() == 1);
    for (auto [p, callable_identifier] : parameters[0]) {
      auto [iter, inserted] = parameters_options.emplace(p, &id);
      // TODO: Error: ambiguity? There are other forms of ambiguity too
      // though.
      ASSERT(inserted);
    }
  }
  co_yield tv.ParametersOf(node, std::move(parameters_options));

  for (auto const &id : scope.visible_ancestor_declaration_id_named(name)) {
    std::span qts = co_await VerifyTypeOf(&id);
    ASSERT(qts.size() == 1);

    if (qts[0].qualifiers() >= Qualifiers::Error()) {
      co_return tv.TypeOf(node, Error());
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

      tv.context().set_symbol(node, symbol_ref);

      if (qt.qualifiers() >= Qualifiers::Error()) {
        co_return tv.TypeOf(node, qt);
      }

      if (not(qt.qualifiers() >= Qualifiers::Constant())) {
        qt = Reference(qt);
      }

      co_return tv.TypeOf(node, qt);
    }
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
      co_return tv.TypeOf(node, qt);
    }
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

      if (error) { co_return tv.TypeOf(node, qt); }

      // TODO: Implement correctly.
      co_return tv.TypeOf(node, qt);
    }
  }
}

}  // namespace semantic_analysis
