#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/cyclic_dependency_tracker.h"
#include "compiler/module.h"
#include "compiler/verify/common.h"
#include "compiler/verify/verify.h"
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
            .Highlighted(use_view.range(), diagnostic::Style::ErrorText())
            .Highlighted(id_view.range(), diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  frontend::SourceView id_view;
  frontend::SourceView use_view;
};

struct UncapturedIdentifier {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncaptured-identifier";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Found an identifier '%s' which is not visible in the "
                         "current scope:",
                         id),
        diagnostic::SourceQuote()
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  frontend::SourceView view;
};

struct PotentialIdentifiers {
  using symbol_ref_t = base::PtrUnion<ast::Declaration::Id const,
                                      module::Module::SymbolInformation const>;
  std::vector<std::pair<symbol_ref_t, type::QualType>> viable;
  std::vector<ast::Declaration::Id const *> errors;
  std::vector<ast::Declaration::Id const *> unreachable;
};

// Returns the declaration ids along with their qualified type that may be
// referenced by the given identifier, or nullopt if any one of them is an
// error.
PotentialIdentifiers PotentialIds(CompilationDataReference data,
                                  ast::Identifier const &id) {
  PotentialIdentifiers result;
  bool only_constants = false;
  for (ast::Scope const &s : id.scope()->ancestors()) {
  if (auto iter = s.decls_.find(id.name()); iter != s.decls_.end()) {
    for (auto const *id : iter->second) {
      auto const *decl_id_qt = data.context().maybe_qual_type(id).data();
      type::QualType qt = decl_id_qt ? *decl_id_qt : VerifyType(data, id)[0];

      if (not qt.ok()) {
        result.errors.push_back(id);
      } else {
        if (only_constants and
            not(id->declaration().flags() & ast::Declaration::f_IsConst)) {
          result.unreachable.push_back(id);
        } else {
          result.viable.emplace_back(id, qt);
        }
      }
    }
    }

    for (auto *mod : s.embedded_modules()) {
      for (auto const &symbol : mod->Exported(id.name())) {
        result.viable.emplace_back(&symbol, symbol.qualified_type);
      }
    }

    if (s.kind() == ast::Scope::Kind::BoundaryExecutable) {
      only_constants = true;
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

  // Dependency pushed until `token` is destroyed.
  auto token = state().cyclic_dependency_tracker.PushDependency(node, diag());
  if (not token) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  // TODO: In what circumstances could this have been seen more than once?
  if (auto qts = context().maybe_qual_type(node); qts.data()) { return qts; }

  auto [viable, errors, unreachable] = PotentialIds(*this, *node);

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
        //       .id_view  = SourceViewFor(potential_id.first),
        //       .use_view = SourceViewFor(node),
        //   });
        //   // Haven't seen the declaration yet, so we can't proceed.
        //   return context().set_qual_type(node, type::QualType::Error());
        // } else {
          qt = symbol_qt;
        //}

        if (qt.HasErrorMark()) { return context().set_qual_type(node, qt); }

        if (qt.type().is<type::Array>()) {
          qt = type::QualType(qt.type(), qt.quals() | type::Quals::Buf());
        }

        // TODO: shouldn't need to reconstruct just to set the quals.
        qt = type::QualType(qt.type(), qt.quals() | type::Quals::Ref());
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
      // TODO: Performance. We don't need to look at these, we just need to know
      // if any exist.
      bool present = false;
      for (auto *s = node->scope(); s; s = s->parent()) {
        if (s->decls_.contains(node->name())) {
          present = true;
          break;
        }
      }
      if (present) {
        diag().Consume(UncapturedIdentifier{
            .id   = node->name(),
            .view = SourceViewFor(node),
        });
      } else {
        diag().Consume(UndeclaredIdentifier{
            .id   = node->name(),
            .view = SourceViewFor(node),
        });
      }
      return context().set_qual_type(node, type::QualType::Error());
    } break;
    default: {
      type::Quals quals = type::Quals::Const();
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
