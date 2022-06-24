#include "absl/cleanup/cleanup.h"
#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/common_diagnostics.h"
#include "compiler/module.h"
#include "compiler/verify/common.h"
#include "type/cast.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

struct DeclaringHoleAsNonModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "declaring-hole-as-non-module";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Declaring `--` as non-module type `%s`.", type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string type;
  std::string_view view;
};

struct EmbeddingNonConstantModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "embedding-non-constant-module";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Declaring `--` as non-constant module."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct ShadowingDeclaration {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "shadowing-declaration";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Ambiguous declarations:"),
        diagnostic::SourceQuote()
            .Highlighted(view1, diagnostic::Style{})
            .Highlighted(view2, diagnostic::Style{}));
  }

  std::string_view view1;
  std::string_view view2;
};

struct NoDefaultValue {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-default-value";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("There is no default value for the type `%s`.", type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string type;
  std::string_view view;
};

struct NonConstantTypeInDeclaration {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "non-constant-type-in-declaration";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Non-constant type encountered in declaration."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct UninitializedConstant {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uninitialized-constant";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to define a constant with an uninitialized value."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

bool Shadows(type::Type t1, type::Type t2) {
  // TODO: Don't worry about generic shadowing? It'll be checked later?
  if (t1.is<type::Generic<type::Function>>() or
      t2.is<type::Generic<type::Function>>() or t1 == type::UnboundScope or
      t2 == type::UnboundScope) {
    return false;
  }

  type::Callable const *callable1 = t1.if_as<type::Callable>();
  type::Callable const *callable2 = t2.if_as<type::Callable>();
  if (not callable1 or not callable2) { return true; }

  return core::AmbiguouslyCallable(
      callable1->parameters(), callable2->parameters(),
      [](type::QualType lhs, type::QualType rhs) {
        return type::Meet(lhs.type(), rhs.type()) != nullptr;
      });
}

// Verifies and evaluates the type expression, returning its value if it can be
// computed or an error.
type::QualType VerifyDeclarationType(CompilationDataReference data,
                                     ast::Declaration const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto type_expr_qt, VerifyType(data, node->type_expr())[0]);
  if (type_expr_qt.type() == type::Type_) {
    if (not type_expr_qt.constant()) {
      data.diag().Consume(NonConstantTypeInDeclaration{
          .view = node->type_expr()->range(),
      });
      return type::QualType::Error();
    }

    ASSIGN_OR(return type::QualType::Error(),  //
                     auto t,
                     data.EvaluateOrDiagnoseAs<type::Type>(node->type_expr()));

    return type::QualType(t, (node->flags() & ast::Declaration::f_IsConst)
                                 ? type::Qualifiers::Constant()
                                 : type::Qualifiers::Unqualified());
  } else {
    // TODO: Not a type or *INTERFACE*
    data.diag().Consume(NotAType{
        .view = node->type_expr()->range(),
        .type = TypeForDiagnostic(node->type_expr(), data.context()),
    });
    return type::QualType::Error();
  }
}

// Verifies the type of a declaration of the form `x: t`.
type::QualType VerifyDefaultInitialization(CompilationDataReference data,
                                           ast::Declaration const *node) {
  ASSIGN_OR(return type::QualType::Error(), auto qt,
                   VerifyDeclarationType(data, node));
  if (not(node->flags() & ast::Declaration::f_IsFnParam) and
      not qt.type().get()->IsDefaultInitializable()) {
    data.diag().Consume(NoDefaultValue{
        .type = TypeForDiagnostic(node, data.context()),
        .view = node->range(),
    });
  }
  return qt;
}

bool VerifyInitialization(diagnostic::DiagnosticConsumer &diag,
                          std::string_view view, type::QualType to,
                          type::QualType from) {
  if (not type::CanCastImplicitly(from.type(), to.type())) {
    // TODO: Wire through the expressions relevant to this type so we can emit
    // better error messages.
    diag.Consume(InvalidCast{.from = from.type().to_string(),
                             .to   = to.type().to_string(),
                             .view = view});
    return false;
  } else {
    return true;
  }
}

// Verifies the type of a declaration of the form `x := y`.
std::vector<type::QualType> VerifyInferred(CompilationDataReference data,
                                           ast::Declaration const *node) {
  auto init_val_qt_span = VerifyType(data, node->init_val());
  std::vector<type::QualType> init_val_qts(init_val_qt_span.begin(),
                                           init_val_qt_span.end());

  type::Qualifiers quals = (node->flags() & ast::Declaration::f_IsConst)
                               ? type::Qualifiers::Constant()
                               : type::Qualifiers::Unqualified();

  bool inference_failure = false;
  for (auto &qt : init_val_qts) {
    if (auto inference_result = Inference(qt.type())) {
      if (not(quals >= type::Qualifiers::Constant())) {
        bool had_error_mark = qt.HasErrorMark();
        qt                  = type::QualType(*inference_result, quals);
        if (had_error_mark) { qt.MarkError(); }
      }
    } else {
      data.diag().Consume(type::UninferrableType{
          .kind = inference_result.failure(),
          .view = node->init_val()->range(),
      });
      inference_failure = true;
    }
  }

  if (inference_failure) { return {type::QualType::Error()}; }

  for (auto &qt : init_val_qts) {
    if (not VerifyInitialization(data.diag(), node->range(), qt, qt)) {
      qt.MarkError();
    }
    qt.set_quals(quals);
  }

  return init_val_qts;
}

// Verifies the type of a declaration of the form `x: T = y`.
type::QualType VerifyCustom(TypeVerifier &tv, ast::Declaration const *node) {
  auto init_val_qt = tv.VerifyType(node->init_val())[0];

  ASSIGN_OR(return type::QualType::Error(), auto qt,
                   VerifyDeclarationType(tv, node));

  if (not init_val_qt.ok() or
      not VerifyInitialization(tv.diag(), node->range(), qt, init_val_qt)) {
    qt.MarkError();
  }

  return qt;
}

type::QualType VerifyUninitialized(TypeVerifier &tv,
                                   ast::Declaration const *node) {
  if (node->flags() & ast::Declaration::f_IsConst) {
    tv.diag().Consume(UninitializedConstant{.view = node->range()});
  }

  return VerifyDeclarationType(tv, node);
}

// Returns the qualified types corresponding to a constant declaration `node` if
// they have been previously verified, otherwise returns a span whose data is
// null.
absl::Span<type::QualType const> PreviouslyVerifiedConstant(
    TypeVerifier &tv, ast::Declaration const *node) {
  if (not(node->flags() & ast::Declaration::f_IsConst)) { return {}; }
  if (auto qts = tv.context().maybe_qual_type(&node->ids()[0]); qts.data()) {
    return qts;
  }
  return {};
}

std::string DeclarationIdsString(absl::Span<ast::Declaration::Id const> ids) {
  return absl::StrJoin(ids, ", ",
                       [](std::string *out, ast::Declaration::Id const &id) {
                         absl::StrAppend(out, id.name());
                       });
}

// Returns a `std::vector` consisiting of the `type::QualTypes` for each
// `ast::Declaration::Id`, possibly having some or all of the elements be
// errors. The length of the returned `std::vector` must be
// `node->ids().size()`.
std::vector<type::QualType> ComputeQualTypes(TypeVerifier &tv,
                                             ast::Declaration const *node) {
  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      return {VerifyDefaultInitialization(tv, node)};
    } break;
    case ast::Declaration::kInferred: {
      return VerifyInferred(tv, node);
    } break;
    case ast::Declaration::kInferredAndUninitialized: {
      tv.diag().Consume(type::UninferrableType{
          .kind = type::InferenceResult::Kind::Uninitialized,
          .view = node->init_val()->range(),
      });
      if (node->flags() & ast::Declaration::f_IsConst) {
        tv.diag().Consume(UninitializedConstant{.view = node->range()});
      }
      return {type::QualType::Error()};
    } break;
    case ast::Declaration::kCustomInit: {
      return {VerifyCustom(tv, node)};
    } break;
    case ast::Declaration::kUninitialized: {
      return {VerifyUninitialized(tv, node)};
    } break;
    case ast::Declaration::kBinding: {
      tv.VerifyType(&node->as<ast::BindingDeclaration>().pattern());
      absl::Span span = tv.context().qual_types(node);
      return std::vector<type::QualType>(span.begin(), span.end());
    } break;
    default: UNREACHABLE(node->DebugString());
  }
}

absl::Span<type::QualType const> StoreQualTypes(
    Context &ctx, ast::Declaration const *node,
    absl::Span<type::QualType const> qual_types) {
  ASSERT(qual_types.size() == node->ids().size());
  for (size_t i = 0; i < qual_types.size(); ++i) {
    ctx.set_qual_type(&node->ids()[i], qual_types[i]);
  }
  return ctx.set_qual_types(node, qual_types);
}

bool VerifyEmbeddedIds(TypeVerifier &tv, ast::Declaration const *node,
                       absl::Span<type::QualType> qual_types) {
  bool found_fatal_error = false;
  ASSERT(qual_types.size() == node->ids().size());
  for (size_t i = 0; i < qual_types.size(); ++i) {
    auto &qt       = qual_types[i];
    auto const &id = node->ids()[i];

    // Embedded declaration identifiers are expressed by having no name.
    if (not id.name().empty()) { continue; }

    // Only constant modules may be embedded.
    if (qt.type() != type::Module) {
      tv.diag().Consume(DeclaringHoleAsNonModule{
          .type = TypeForDiagnostic(node, tv.context()),
          .view = node->range(),
      });
      found_fatal_error = true;
      qt.MarkError();
      continue;
    }

    if (not qt.constant()) {
      tv.diag().Consume(EmbeddingNonConstantModule{.view = node->range()});
      found_fatal_error = true;
      qt.MarkError();
      continue;
    }

    // TODO: What if no init val is provded? what if not constant?

    if (auto maybe_mod =
            tv.EvaluateOrDiagnoseAs<ir::ModuleId>(node->init_val())) {
      // TODO: In generic contexts it doesn't make sense to place this on
      // the AST.
      node->scope()->embed(&tv.importer().get(*maybe_mod).as<CompiledModule>());
    } else {
      found_fatal_error = true;
      qt.MarkError();
    }
  }
  return not found_fatal_error;
}

void VerifyLocalShadowing(TypeVerifier &tv, ast::Declaration const *node,
                          absl::Span<type::QualType> qual_types) {
  for (size_t i = 0; i < qual_types.size(); ++i) {
    auto &qt       = qual_types[i];
    auto const &id = node->ids()[i];
    if (id.name().empty()) { continue; }

    ast::Scope const &s = *node->scope();

    for (auto const &decl_id :
         s.visible_ancestor_declaration_id_named(id.name())) {
      if (&id == &decl_id) { continue; }

      auto decl_id_qts = tv.context().maybe_qual_type(&decl_id);
      if (not decl_id_qts.data()) { continue; }

      if (Shadows(qt.type(), decl_id_qts[0].type())) {
        qt.MarkError();

        tv.diag().Consume(ShadowingDeclaration{
            .view1 = id.range(),
            .view2 = decl_id.range(),
        });
      }
    }
  }
}

void VerifyEmbeddedShadowing(TypeVerifier &tv, std::string_view name,
                             std::string_view id_range, type::QualType &qt,
                             ast::Scope const &s) {
  for (auto const &decl_id : s.visible_ancestor_declaration_id_named("")) {
    auto decl_id_qts = tv.context().maybe_qual_type(&decl_id);
    if (not decl_id_qts.data()) { continue; }
    ASSERT(decl_id_qts.size() == 1);
    ASSERT(decl_id_qts[0] == type::QualType::Constant(type::Module));

    auto maybe_mod =
        tv.EvaluateOrDiagnoseAs<ir::ModuleId>(decl_id.declaration().init_val());
    if (not maybe_mod) { continue; }

    auto const *m =
        ASSERT_NOT_NULL(tv.shared_context().module_table().module(*maybe_mod));
    for (auto const &symbol_information : m->Exported(name)) {
      if (Shadows(qt.type(), symbol_information.qualified_type.type())) {
        qt.MarkError();

        tv.diag().Consume(ShadowingDeclaration{
            .view1 = id_range,
            .view2 = "",
        });
      }
    }
  }
}

void VerifyEmbeddedShadowing(TypeVerifier &tv, ast::Declaration const *node,
                             absl::Span<type::QualType> qual_types) {
  ast::Scope const &s = *node->scope();
  for (size_t i = 0; i < qual_types.size(); ++i) {
    auto &qt       = qual_types[i];
    auto const &id = node->ids()[i];

    if (id.name().empty()) {
      auto decl_id_qts = tv.context().maybe_qual_type(&id);
      auto maybe_mod =
          tv.EvaluateOrDiagnoseAs<ir::ModuleId>(id.declaration().init_val());
      ASSERT(maybe_mod != std::nullopt);

      auto const *m = ASSERT_NOT_NULL(
          tv.shared_context().module_table().module(*maybe_mod));
      m->ExportedSymbolsByName(
          [&](std::string_view symbol_name, auto const &exported_range) {
            for (auto const &symbol_information : exported_range) {
              auto qt = symbol_information.qualified_type;
              // Note: `qt` is copied. The copy is potentially modified by
              // `VerifyEmbeddedShadowing` (by marking an error on it), but
              // there is no need to keep that modification around for the
              // future as it doesn't indicate any failure that should stop
              // further processing.
              VerifyEmbeddedShadowing(tv, symbol_name, "", qt, s);
            }
          });
    } else {
      VerifyEmbeddedShadowing(tv, id.name(), id.range(), qt, s);
    }
  }
}

}  // namespace

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Declaration const *node) {
  // Declarations can be seen out of order if they're constants and we happen to
  // verify an identifier referencing the declaration before the declaration is
  // processed in source-order. Due to the possibility of out-of-order
  // verification, `VerifyType` may be called multiple times on an
  // `ast::Declaration`. To avoid repeating work, we first check (for constants)
  // if we have already verified this declaration previously.
  if (auto qts = PreviouslyVerifiedConstant(*this, node); qts.data()) {
    return qts;
  }

  LOG("Declaration", "Verifying '%s' on %p", DeclarationIdsString(node->ids()),
      &context());

  std::vector node_qual_types = ComputeQualTypes(*this, node);
  ASSERT(node_qual_types.size() == node->ids().size());

  for (type::QualType qt : node_qual_types) {
    if (qt == type::QualType::Error()) {
      return StoreQualTypes(context(), node, node_qual_types);
    }
  }

  VerifyLocalShadowing(*this, node, absl::MakeSpan(node_qual_types));
  if (VerifyEmbeddedIds(*this, node, absl::MakeSpan(node_qual_types))) {
    VerifyEmbeddedShadowing(*this, node, absl::MakeSpan(node_qual_types));
  }
  return StoreQualTypes(context(), node, node_qual_types);
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::Declaration::Id const *node) {
  // This is never called directly, but may be called if an `Identifier` is
  // being verified whose declaration has not yet been seen.
  return VerifyType(&node->declaration());
}

absl::Span<type::QualType const> TypeVerifier::VerifyType(
    ast::BindingDeclaration const *node) {
  UNREACHABLE();
}

bool PatternTypeVerifier::VerifyPatternType(ast::Declaration const *node,
                                            type::Type t) {
  UNREACHABLE(node->DebugString());
}

bool PatternTypeVerifier::VerifyPatternType(ast::BindingDeclaration const *node,
                                            type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));
  // TODO: Do this explicitly on VerifyPatternType(Declaration::Id, type::Type)
  for (auto const &id : node->ids()) {
    context().set_qual_type(&id, type::QualType::Constant(t));
  }
  return true;
}

}  // namespace compiler
