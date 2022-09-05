#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/array.h"
#include "type/overload_set.h"
#include "type/qual_type.h"

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

std::vector<ast::Declaration::Id const *> DeclarationIds(
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
  std::vector<std::pair<symbol_ref_type, type::QualType>> viable;

  auto &scope = *node->scope();
  std::string_view name = node->name();
  for (auto const &id : scope.visible_ancestor_declaration_id_named(name)) {
    absl::Span<type::QualType const> qts = co_await VerifyTypeOf(&id);
    ASSERT(qts.size() == 1);
    if (qts[0].ok()) {
      viable.emplace_back(&id, qts[0]);
    } else {
      tv.complete_verification(node, type::QualType::Error());
      co_return;
    }
  }

  for (ast::Scope const &s : scope.ancestors()) {
    for (auto *mod : s.embedded_modules()) {
      for (auto const &symbol : mod->Exported(name)) {
        viable.emplace_back(&symbol, symbol.qualified_type);
      }
    }
  }

  type::QualType qt;
  switch (viable.size()) {
    case 1: {
      auto const &[symbol_ref, symbol_qt] = viable[0];
      qt                                  = symbol_qt;

      if (qt.HasErrorMark()) {
        tv.complete_verification(node, qt);
        co_return;
      }

      if (not symbol_qt.constant()) {
        if (qt.type().is<type::Array>()) {
          qt = type::QualType(qt.type(),
                              qt.quals() | type::Qualifiers::Buffer());
        }

        qt |= type::Qualifiers::Storage();
      }

      // TODO: SetCallMetadata for callables.
      tv.context().set_decls(node, DeclarationIds(symbol_ref));
      ASSERT(qt.type().valid() == true);
      tv.complete_verification(node, qt);
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
      qt = type::QualType::Error();
      tv.complete_verification(node, qt);
    } break;
    default: {
      type::Qualifiers quals = type::Qualifiers::Constant();
      absl::flat_hash_set<type::Type> member_types;
      bool error = false;

      for (auto const &[symbol_ref, id_qt] : viable) {
        qt = id_qt;
        if (qt.HasErrorMark()) { error = true; }
        quals &= qt.quals();
        member_types.insert(qt.type());
      }

      if (error) {
        tv.complete_verification(node, qt);
        co_return;
      }

      // TODO: The conversion here from Decl::Id to Expression indicates
      // caller_locator_t should become more specific.
      absl::flat_hash_set<compiler::CallMetadata::callee_locator_t>
          potential_ids;
      std::vector<ast::Declaration::Id const *> decl_ids;
      potential_ids.reserve(viable.size());
      for (auto const &[symbol_ref, id_qt] : viable) {
        if (auto const *id = symbol_ref.get_if<ast::Declaration::Id>()) {
          potential_ids.insert(static_cast<ast::Expression const *>(id));
          decl_ids.push_back(id);
        } else {
          potential_ids.insert(
              symbol_ref.get<module::Module::SymbolInformation>());

          decl_ids.push_back(
              symbol_ref.get<module::Module::SymbolInformation>()->id);
        }
      }

      tv.context().SetCallMetadata(
          node, compiler::CallMetadata(std::move(potential_ids)));
      tv.context().set_decls(node, std::move(decl_ids));
      qt = type::QualType(type::MakeOverloadSet(member_types), quals);
      ASSERT(qt.type().valid() == true);
      tv.complete_verification(node, qt);
    } break;
  }
}

}  // namespace semantic_analysis
