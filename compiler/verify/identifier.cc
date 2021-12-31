#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/compiler.h"
#include "compiler/cyclic_dependency_tracker.h"
#include "compiler/module.h"
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
        diagnostic::SourceQuote(&use_view.buffer())
            .Highlighted(use_view.range(), diagnostic::Style::ErrorText())
            .Highlighted(id_view.range(), diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  frontend::SourceView id_view;
  frontend::SourceView use_view;
};

struct UndeclaredIdentifier {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "undeclared-identifier";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Found an undeclared identifier '%s':", id),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  frontend::SourceView view;
};

struct UncapturedIdentifier {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uncaptured-identifier";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Found an identifier '%s' which is not visible in the "
                         "current scope:",
                         id),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  frontend::SourceView view;
};

// Returns the declaration ids along with their qualified type that may be
// referenced by the given identifier, or nullopt if any one of them is an
// error.
std::optional<
    std::vector<std::pair<ast::Declaration::Id const *, type::QualType>>>
PotentialIds(Compiler &c, ast::Identifier const &id) {
  std::optional<
      std::vector<std::pair<ast::Declaration::Id const *, type::QualType>>>
      result(std::in_place);

  for (auto const *decl_id :
       module::AllVisibleDeclsTowardsRoot(id.scope(), id.name())) {
    type::QualType qt;
    if (auto const *decl_id_qt = c.context().maybe_qual_type(decl_id).data()) {
      qt = *decl_id_qt;
    } else {
      auto const *mod = &ModuleFor(decl_id)->as<CompiledModule>();
      if (mod != c.resources().module) {
        qt = mod->context().qual_types(decl_id)[0];
      } else {
        // TODO: Eventually we may want to relax this for functions where we
        // don't need the entire decl we just need to know if it's callable.
        qt = c.VerifyType(decl_id)[0];
      }

      if (not qt.ok()) {
        // Rather than returning immediately, we continue processing in case
        // further calls to VerifyType help us collect more error messages,
        result = std::nullopt;
      }
    }
    if (result) { result->emplace_back(decl_id, qt); }
  }

  if (result) {
    // TODO: Can there be any ADL modules?
    if (auto const *adl_modules = c.context().AdlModules(&id)) {
      for (auto const *mod : *adl_modules) {
        auto ids = mod->scope().ExportedDeclarationIds(id.name());
        for (auto const *decl_id : ids) {
          result->emplace_back(
              decl_id, mod->context().qual_types(&decl_id->declaration())[0]);
        }
      }
    }
  }

  return result;
}

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::Identifier const *node) {
  if (cylcic_dependency_tracker_.has_error(node)) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  // Dependency pushed until `token` is destroyed.
  auto token = cylcic_dependency_tracker_.PushDependency(node, diag());
  if (not token) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  // TODO: In what circumstances could this have been seen more than once?
  if (auto qts = context().maybe_qual_type(node); qts.data()) { return qts; }

  auto potential_decl_ids = PotentialIds(*this, *node);

  LOG("Identifier", "%s: %p %s", node->DebugString(), node, potential_decl_ids);

  if (not potential_decl_ids) {
    return context().set_qual_type(node, type::QualType::Error());
  }

  type::QualType qt;
  switch (potential_decl_ids->size()) {
    case 1: {
      auto const &potential_id = (*potential_decl_ids)[0];
      auto const &[id, id_qt]  = potential_id;
      auto const *decl         = &id->declaration();
      if (decl->flags() & ast::Declaration::f_IsConst) {
        qt = id_qt;
        if (not qt.ok() or qt.HasErrorMark()) {
          return context().set_qual_type(node, qt);
        }
      } else {
        if (node->range().begin() < id->range().begin()) {
          diag().Consume(DeclOutOfOrder{
              .id       = node->name(),
              .id_view  = SourceViewFor(potential_id.first),
              .use_view = SourceViewFor(node),
          });
          // Haven't seen the declaration yet, so we can't proceed.
          return context().set_qual_type(node, type::QualType::Error());
        } else {
          qt = context().qual_types(id)[0];
        }

        if (not qt.ok() or qt.HasErrorMark()) {
          return context().set_qual_type(node, qt);
        }

        if (not qt.constant()) {
          if (qt.type().is<type::Array>()) {
            qt = type::QualType(qt.type(), qt.quals() | type::Quals::Buf());
          }

          // TODO: shouldn't need to reconstruct just to set the quals.
          qt = type::QualType(qt.type(), qt.quals() | type::Quals::Ref());
        }
      }

      if (qt.type().is<type::Callable>() or
          qt.type().is<type::Generic<type::Function>>()) {
        context().SetAllOverloads(node, ast::OverloadSet({id}));
      }

      LOG("Identifier", "setting %s: %s", node->name(), qt);
      std::vector<ast::Declaration::Id const *> decl_ids;
      for (auto const &[id, id_qt] : *potential_decl_ids) {
        decl_ids.push_back(id);
      }
      context().set_decls(node, std::move(decl_ids));
    } break;
    case 0: {
      // TODO: Performance. We don't need to look at these, we just need to know
      // if any exist.

      bool present = false;
      node->scope()->ForEachDeclIdTowardsRoot(
          node->name(), [&](ast::Declaration::Id const *id) {
            present = true;
            return false;
          });
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

      for (auto const &[id, id_qt] : *potential_decl_ids) {
        qt = id_qt;
        if (not qt.ok() or qt.HasErrorMark()) { error = true; }
        quals &= qt.quals();
        member_types.insert(qt.type());
      }

      if (error) {
        return context().set_qual_type(node, type::QualType::Error());
      }

      std::vector<ast::Declaration::Id const *> potential_ids;
      potential_ids.reserve(potential_decl_ids->size());
      for (auto const &[id, id_qt] : *potential_decl_ids) {
        potential_ids.push_back(id);
      }
      context().SetAllOverloads(node, ast::OverloadSet(potential_ids));
      qt = type::QualType(type::MakeOverloadSet(member_types), quals);
      LOG("Identifier", "setting %s", node->name());
      std::vector<ast::Declaration::Id const *> decl_ids;
      for (auto const &[id, id_qt] : *potential_decl_ids) {
        decl_ids.push_back(id);
      }
      context().set_decls(node, std::move(decl_ids));
    } break;
  }

  ASSERT(qt.type().valid() == true);
  LOG("Identifier", "setting %s: %s", node->name(), qt);
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
