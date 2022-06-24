#include "absl/cleanup/cleanup.h"
#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/module.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
#include "core/cycle_tracker.h"
#include "type/array.h"
#include "type/overload_set.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

namespace {

struct DeclOutOfOrder {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "declaration-out-of-order";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Variable `%s` used before it was declared.", id),
        diagnostic::SourceQuote()
            .Highlighted(use_view, diagnostic::Style::ErrorText())
            .Highlighted(id_view, diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  std::string_view id_view;
  std::string_view use_view;
};

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

struct CyclicIdentifierDependency {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "cyclic-dependency";

  diagnostic::DiagnosticMessage ToMessage() const {
    diagnostic::SourceQuote quote;
    for (auto const &view : cycle) {
      quote = quote.Highlighted(view, diagnostic::Style::ErrorText());
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Found a cyclic dependency:"), std::move(quote));
  }

  std::vector<std::string_view> cycle;
};

struct PotentialIdentifiers {
  using symbol_ref_t = base::PtrUnion<ast::Declaration::Id const,
                                      module::Module::SymbolInformation const>;
  std::vector<std::pair<symbol_ref_t, type::QualType>> viable;
  std::vector<ast::Declaration::Id const *> errors;
};

// Returns the declaration ids along with their qualified type that may be
// referenced by the given identifier, or nullopt if any one of them is an
// error.
PotentialIdentifiers PotentialIds(CompilationDataReference data,
                                  ast::Identifier const &id) {
  PotentialIdentifiers result;
  for (auto const &id :
       id.scope()->visible_ancestor_declaration_id_named(id.name())) {
    auto const *decl_id_qt = data.context().maybe_qual_type(&id).data();
    type::QualType qt = decl_id_qt ? *decl_id_qt : VerifyType(data, &id)[0];
    if (qt.ok()) {
      result.viable.emplace_back(&id, qt);
    } else {
      result.errors.push_back(&id);
    }
  }

  for (ast::Scope const &s : id.scope()->ancestors()) {
    for (auto *mod : s.embedded_modules()) {
      for (auto const &symbol : mod->Exported(id.name())) {
        result.viable.emplace_back(&symbol, symbol.qualified_type);
      }
    }
  }

  return result;
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Identifier const *node) {
  if (state().cyclic_dependency_tracker.has_error(node)) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  bool has_cyclic_dep = false;
  state().cyclic_dependency_tracker.push(
      node, [&](absl::Span<ast::Identifier const *const> ids) {
        has_cyclic_dep = true;
        std::vector<std::string_view> cycle;
        cycle.reserve(ids.size());
        for (auto const *id : ids) { cycle.push_back(id->range()); }
        diag().Consume(CyclicIdentifierDependency{.cycle = std::move(cycle)});
      });
  if (has_cyclic_dep) {
    return context().set_qual_type(node, type::QualType::Error());
  }
  absl::Cleanup cleanup = [&] { state().cyclic_dependency_tracker.pop(); };

  // TODO: In what circumstances could this have been seen more than once?
  if (auto qts = context().maybe_qual_type(node); qts.data()) { return qts; }

  auto [viable, errors] = PotentialIds(*this, *node);

  if (not errors.empty()) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  type::QualType qt;
  switch (viable.size()) {
    case 1: {
      auto const &[symbol_ref, symbol_qt] = viable[0];
      if (symbol_qt.constant()) {
        qt = symbol_qt;
        if (qt.HasErrorMark()) { return context().set_qual_type(node, qt); }
      } else {
        // TODO: Use `ordered_decls` on the scope or something similar to make
        // this less gross.
        // if (node->range().begin() < id->range().begin()) {
        //   diag().Consume(DeclOutOfOrder{
        //       .id       = node->name(),
        //       .id_view  = potential_id.first->range(),
        //       .use_view = node->range(),
        //   });
        //   // Haven't seen the declaration yet, so we can't proceed.
        //   return context().set_qual_type(node, type::QualType::Error());
        // } else {
        qt = symbol_qt;
        //}

        if (qt.HasErrorMark()) { return context().set_qual_type(node, qt); }

        if (qt.type().is<type::Array>()) {
          qt = type::QualType(qt.type(),
                              qt.quals() | type::Qualifiers::Buffer());
        }

        // TODO: shouldn't need to reconstruct just to set the quals.
        qt =
            type::QualType(qt.type(), qt.quals() | type::Qualifiers::Storage());
      }

      if (qt.type().is<type::Callable>() or
          qt.type().is<type::Generic<type::Function>>() or
          qt.type().is<type::Generic<type::Block>>()) {
        absl::flat_hash_set<CallMetadata::callee_locator_t> set;
        if (auto const *id = symbol_ref.get_if<ast::Declaration::Id>()) {
          set.insert(static_cast<ast::Expression const *>(id));
        } else {
          set.insert(symbol_ref.get<module::Module::SymbolInformation>());
        }
        context().SetCallMetadata(node, CallMetadata(std::move(set)));
      }

      LOG("Identifier", "setting %s: %s", node->name(), qt);
      std::vector<ast::Declaration::Id const *> v;
      if (auto const *id = symbol_ref.get_if<ast::Declaration::Id>()) {
        v.push_back(id);
      } else {
        v.push_back(symbol_ref.get<module::Module::SymbolInformation>()->id);
      }
      context().set_decls(node, std::move(v));
    } break;
    case 0: {
      if (std::empty(
              node->scope()->ancestor_declaration_id_named(node->name()))) {
        diag().Consume(UndeclaredIdentifier{
            .id   = node->name(),
            .view = node->range(),
        });
      } else {
        diag().Consume(UncapturedIdentifier{
            .id   = node->name(),
            .view = node->range(),
        });
      }
      return context().set_qual_type(node, type::QualType::Error());
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
        return context().set_qual_type(node, type::QualType::Error());
      }

      // TODO: The conversion here from Decl::Id to Expression indicates
      // caller_locator_t should become more specific.
      absl::flat_hash_set<CallMetadata::callee_locator_t> potential_ids;
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
      LOG("Identifier", "setting %s", node->name());

      context().SetCallMetadata(node, CallMetadata(std::move(potential_ids)));
      context().set_decls(node, std::move(decl_ids));
      qt = type::QualType(type::MakeOverloadSet(member_types), quals);
    } break;
  }

  ASSERT(qt.type().valid() == true);
  LOG("Identifier", "setting %s: %s", node->name(), qt);
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
