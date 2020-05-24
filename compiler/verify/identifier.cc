#include "ast/ast.h"
#include "base/defer.h"
#include "compiler/compiler.h"
#include "type/overload_set.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {

namespace {

struct CyclicDependency {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "cyclic-dependency";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    diagnostic::SourceQuote quote(src);
    for (auto const *id : cycle) {
      quote = quote.Highlighted(id->range(), diagnostic::Style::ErrorText());
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Found a cyclic dependency:"), std::move(quote));
  }

  std::vector<ast::Identifier const *> cycle;
};

struct DeclOutOfOrder {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "declaration-out-of-order";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Variable `%s` used before it was declared.", id),
        diagnostic::SourceQuote(src)
            .Highlighted(use_range, diagnostic::Style::ErrorText())
            .Highlighted(id_range, diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  frontend::SourceRange id_range;
  frontend::SourceRange use_range;
};

struct UndeclaredIdentifier {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "undeclared-identifier";

   diagnostic:: DiagnosticMessage ToMessage(frontend::Source const *src) const {
     return diagnostic::DiagnosticMessage(
         diagnostic::Text("Found an undeclared identifier:"),
         diagnostic::SourceQuote(src).Highlighted(
             range, diagnostic::Style::ErrorText()));
  }

  std::string_view id;
  frontend::SourceRange range;
};

struct NonCallableInOverloadSet {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "non-callable-in-overload-set";

  // TODO this assumes a single source for all references in this message.
  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "NonCallable type `%s` in overload set requested here:",
            decl_type->to_string()),
        diagnostic::SourceQuote(src).Highlighted(
            id, diagnostic::Style::ErrorText()),
        diagnostic::Text("Declaration here:"),
        diagnostic::SourceQuote(src).Highlighted(decl, diagnostic::Style{}));
  }

  frontend::SourceRange id;
  frontend::SourceRange decl;
  type::Type const *decl_type;
};

}  // namespace

type::QualType Compiler::VerifyType(ast::Identifier const *node) {
  if (data().cyclic_error(node)) { return type::QualType::Error(); }

  auto cyclic_dep_span = state_.dependency_chain.PushDependency(node);
  base::defer d([&] { state_.dependency_chain.PopDependency(); });

  if (not cyclic_dep_span.empty()) {
    for (auto const *i : cyclic_dep_span) { data().set_cyclic_error(i); }
    diag().Consume(CyclicDependency{
        .cycle = std::vector(cyclic_dep_span.begin(), cyclic_dep_span.end()),
    });

    return type::QualType::Error();
  }

  type::QualType qt;

  auto potential_decls =
      module::AllDeclsTowardsRoot(node->scope(), node->token());
  DEBUG_LOG("Identifier")
  (node->DebugString(), ": ", node, " ", potential_decls);
  switch (potential_decls.size()) {
    case 1: {
      if (potential_decls[0]->flags() & ast::Declaration::f_IsConst) {
        if (auto const *maybe_qt = data().qual_type(potential_decls[0])) {
          qt = *maybe_qt;
        } else {
          ASSIGN_OR(return type::QualType::Error(),  //
                           qt, VerifyType(potential_decls[0]));
        }
      } else {
        if (node->range().begin() < potential_decls[0]->range().begin()) {
          diag().Consume(DeclOutOfOrder{
              .id        = node->token(),
              .id_range  = potential_decls[0]->id_range(),
              .use_range = node->range(),
          });
          // Haven't seen the declaration yet, so we can't proceed.
          return type::QualType::Error();
        } else {
          qt = *ASSERT_NOT_NULL(data().qual_type(potential_decls[0]));
        }
      }

      if (not qt.constant()) {
        // TODO: shouldn't need to reconstruct just to set the quals.
        qt = type::QualType(qt.type(), qt.quals() | type::Quals::Ref());
      }
      data().set_decls(node, std::move(potential_decls));
    } break;
    case 0: {
      diag().Consume(UndeclaredIdentifier{
          .id    = node->token(),
          .range = node->range(),
      });
      return type::QualType::Error();
    } break;
    default: {
      type::Quals quals = type::Quals::Const();
      absl::flat_hash_set<type::Callable const *> member_types;
      bool error = false;
      for (auto const *decl : potential_decls) {
        qt = *ASSERT_NOT_NULL(data().qual_type(decl));
        if (not qt.ok()) { return type::QualType::Error(); }

        if (auto *c = qt.type()->if_as<type::Callable>()) {
          quals &= qt.quals();
          member_types.insert(c);
        } else {
          diag().Consume(NonCallableInOverloadSet{
              .id        = node->range(),
              .decl      = decl->range(),
              .decl_type = qt.type(),
          });
          error = true;
        }
      }

      if (error) { return type::QualType::Error(); }
      qt =
          type::QualType(type::MakeOverloadSet(std::move(member_types)), quals);
      data().set_decls(node, std::move(potential_decls));
    } break;
  }

  ASSERT(qt.type() != nullptr);
  return data().set_qual_type(node, qt);
}

}  // namespace compiler