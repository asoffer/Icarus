#include "absl/cleanup/cleanup.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/module.h"
#include "compiler/verify/common.h"
#include "compiler/verify/internal/assignment_and_initialization.h"
#include "type/qual_type.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

struct DeclaringHoleAsNonModule {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "declaring-hole-as-non-module";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Declaring `--` as non-module type `%s`.", type),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type type;
  frontend::SourceRange range;
};

struct ShadowingDeclaration {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "shadowing-declaration";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Ambiguous declarations:"),
        diagnostic::SourceQuote(src)
            .Highlighted(range1, diagnostic::Style{})
            .Highlighted(range2, diagnostic::Style{}));
  }

  frontend::SourceRange range1;
  frontend::SourceRange range2;
};

struct NoDefaultValue {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-default-value";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("There is no default value for the type `%s`.", type),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type type;
  frontend::SourceRange range;
};

struct NonConstantTypeInDeclaration {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName = "non-constant-type-in-declaration";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Non-constant type encountered in declaration."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct UninitializedConstant {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uninitialized-constant";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Attempting to define a constant with an uninitialized value."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct UninferrableType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "uninferrable-type";

  enum class Reason {
    kInferrable,
    kHole,
    kEmptyArray,
    kNullPtr,
  };

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    char const *text = nullptr;
    switch (reason) {
      case Reason::kInferrable: UNREACHABLE();
      case Reason::kEmptyArray:
        text =
            "Unable to infer the type of the following expression because the "
            "type of an empty array cannot be inferred. Either specify the "
            "type explicitly, or cast it to a specific array type:";
        break;
      case Reason::kNullPtr:
        text =
            "Unable to infer the type of the following expression because the "
            "type of `null` cannot be inferred. Either specify the type "
            "explicitly, or cast it to a specific pointer type:";
        break;
      case Reason::kHole:
        text = "Unable to infer the type of a value that is uninitalized:";
        break;
    }

    return diagnostic::DiagnosticMessage(
        diagnostic::Text(text),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  Reason reason;
  frontend::SourceRange range;
};

UninferrableType::Reason Inferrable(type::Type t) {
  if (t == type::NullPtr) { return UninferrableType::Reason::kNullPtr; }
  if (t == type::EmptyArray) { return UninferrableType::Reason::kEmptyArray; }
  if (auto *a = t.if_as<type::Array>()) { return Inferrable(a->data_type()); }
  if (auto *p = t.if_as<type::Pointer>()) { return Inferrable(p->pointee()); }
  if (auto *f = t.if_as<type::Function>()) {
    for (auto const &param : f->params()) {
      auto reason = Inferrable(param.value.type());
      if (reason != UninferrableType::Reason::kInferrable) { return reason; }
    }
    for (auto t : f->output()) {
      auto reason = Inferrable(t);
      if (reason != UninferrableType::Reason::kInferrable) { return reason; }
    }
  }

  return UninferrableType::Reason::kInferrable;
}

// TODO: what about shadowing of symbols across module boundaries imported with
// -- ::= ?
// Or when you import two modules verifying that symbols don't conflict.
//
// TODO: check shadowing of functions where one has a signature that could be
// created from the other by currying some of the arguments or decide we don't care.
bool Shadow(type::Typed<ast::Declaration::Id const *> id1,
            type::Typed<ast::Declaration::Id const *> id2) {
  type::Callable const *callable1 = id1.type().if_as<type::Callable>();
  type::Callable const *callable2 = id2.type().if_as<type::Callable>();
  if (not callable1 or not callable2) { return true; }

  // TODO: Don't worry about generic shadowing? It'll be checked later?
  if (callable1->is<type::GenericFunction>() or
      callable2->is<type::GenericFunction>()) {
    return false;
  }

  return core::AmbiguouslyCallable(callable1->as<type::Function>().params(),
                                   callable2->as<type::Function>().params(),
                                   [](type::QualType lhs, type::QualType rhs) {
                                     return type::Meet(lhs.type(),
                                                       rhs.type()) != nullptr;
                                   });
}

// Verifies and evaluates the type expression, returning its value if it can be
// computed or an error.
type::QualType VerifyDeclarationType(Compiler &compiler,
                                    ast::Declaration const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto type_expr_qt,
                  compiler.VerifyType(node->type_expr())[0]);
  if (type_expr_qt.type() == type::Type_) {
    if (not type_expr_qt.constant()) {
      compiler.diag().Consume(NonConstantTypeInDeclaration{
          .range = node->type_expr()->range(),
      });
      return type::QualType::Error();
    }

    ASSIGN_OR(
        return type::QualType::Error(),  //
               auto t,
               compiler.EvaluateOrDiagnoseAs<type::Type>(node->type_expr()));

    return type::QualType(t, (node->flags() & ast::Declaration::f_IsConst)
                                 ? type::Quals::Const()
                                 : type::Quals::Unqualified());
  } else if (type_expr_qt.type() == type::Interface) {
    // TODO: Non-constant *INTERFACE*, not a type.
    if (not type_expr_qt.constant()) {
      compiler.diag().Consume(NonConstantTypeInDeclaration{
          .range = node->type_expr()->range(),
      });
      return type::QualType::Error();
    }

    ASSIGN_OR(return type::QualType::Error(),  //
                     auto intf,
                     compiler.EvaluateOrDiagnoseAs<interface::Interface>(
                         node->type_expr()));

    // TODO: we need to pass `intf` in.
    return type::QualType(type::Interface,
                          (node->flags() & ast::Declaration::f_IsConst)
                              ? type::Quals::Const()
                              : type::Quals::Unqualified());
  } else {
    // TODO: Not a type or *INTERFACE*
    compiler.diag().Consume(NotAType{
        .range = node->type_expr()->range(),
        .type  = type_expr_qt.type(),
    });
    return type::QualType::Error();
  }
}

// Verifies the type of a declaration of the form `x: t`.
type::QualType VerifyDefaultInitialization(Compiler &compiler,
                                           ast::Declaration const *node) {
  ASSIGN_OR(return type::QualType::Error(), auto qt,
                   VerifyDeclarationType(compiler, node));

  if (not(node->flags() & ast::Declaration::f_IsFnParam) and
      not qt.type().get()->IsDefaultInitializable()) {
    compiler.diag().Consume(NoDefaultValue{
        .type  = qt.type(),
        .range = node->range(),
    });
  }
  return qt;
}

// Verifies the type of a declaration of the form `x := y`.
std::vector<type::QualType> VerifyInferred(Compiler &compiler,
                                           ast::Declaration const *node) {
  auto init_val_qt_span = compiler.VerifyType(node->init_val());
  std::vector<type::QualType> init_val_qts(init_val_qt_span.begin(),
                                           init_val_qt_span.end());
  bool inference_failure = false;
  // TODO: Support multiple declarations
  auto reason = Inferrable(init_val_qts[0].type());
  if (reason != UninferrableType::Reason::kInferrable) {
    compiler.diag().Consume(UninferrableType{
        .reason = reason,
        .range  = node->init_val()->range(),
    });
    inference_failure = true;
  }

  if (inference_failure) { return {type::QualType::Error()}; }

  type::Quals quals = (node->flags() & ast::Declaration::f_IsConst)
                          ? type::Quals::Const()
                          : type::Quals::Unqualified();
  for (auto &qt : init_val_qts) {
    if (not internal::VerifyInitialization(compiler.diag(), node->range(), qt,
                                           qt)) {
      qt.MarkError();
    }
    qt.set_quals(quals);
  }

  return init_val_qts;
}

// Verifies the type of a declaration of the form `x: T = y`.
type::QualType VerifyCustom(Compiler &compiler, ast::Declaration const *node) {
  auto init_val_qt = compiler.VerifyType(node->init_val())[0];

  ASSIGN_OR(return type::QualType::Error(), auto qt,
                   VerifyDeclarationType(compiler, node));

  if (not init_val_qt.ok() or
      not internal::VerifyInitialization(compiler.diag(), node->range(), qt,
                                         init_val_qt)) {
    qt.MarkError();
  }

  return qt;
}

type::QualType VerifyUninitialized(Compiler &compiler,
                                   ast::Declaration const *node) {
  if (node->flags() & ast::Declaration::f_IsConst) {
    compiler.diag().Consume(UninitializedConstant{.range = node->range()});
  }

  return VerifyDeclarationType(compiler, node);
}

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::Declaration const *node) {
  ASSERT(node->scope()->Containing<ast::ModuleScope>()->module() ==
         &context().module());
  // Declarations can be seen out of order if they're constants and we happen to
  // verify an identifier referencing the declaration before the declaration is
  // processed in source-order.
  //
  // TODO: Consider first checking if it's a constant because we only need to do
  // this lookup in that case. Not sure how much performance that might win.
  if (auto qts = context().maybe_qual_type(&node->ids()[0]); qts.data()) {
    return qts;
  }
  LOG("Declaration", "Verifying '%s' on %p",
      absl::StrJoin(node->ids(), ", ",
                    [](std::string *out, ast::Declaration::Id const &id) {
                      absl::StrAppend(out, id.name());
                    }),
      &context());

  // TODO: If we don't already have type-checked this but it's an error, we'll
  // type-check this node again because we don't save errors. Maybe we should
  // revisit that idea. It's likely the cause of the problem where we generate
  // the same error message multiple times.

  std::vector<type::QualType> node_qual_types;
  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      node_qual_types = {VerifyDefaultInitialization(*this, node)};
    } break;
    case ast::Declaration::kInferred: {
      node_qual_types = VerifyInferred(*this, node);
    } break;
    case ast::Declaration::kInferredAndUninitialized: {
      diag().Consume(UninferrableType{
          .reason = UninferrableType::Reason::kHole,
          .range  = node->init_val()->range(),
      });
      if (node->flags() & ast::Declaration::f_IsConst) {
        diag().Consume(UninitializedConstant{.range = node->range()});
      }
      node_qual_types = {type::QualType::Error()};
    } break;
    case ast::Declaration::kCustomInit: {
      node_qual_types = {VerifyCustom(*this, node)};
    } break;
    case ast::Declaration::kUninitialized: {
      node_qual_types = {VerifyUninitialized(*this, node)};
    } break;
    case ast::Declaration::kBinding: {
      VerifyType(&node->as<ast::BindingDeclaration>().pattern());
      auto span       = context().qual_types(node);
      node_qual_types = std::vector<type::QualType>(span.begin(), span.end());
    } break;
    default: UNREACHABLE(node->DebugString());
  }

  for (type::QualType qt : node_qual_types) {
    if (not qt.ok()) {
      for (auto const &id : node->ids()) {
        context().set_qual_type(&id, type::QualType::Error());
      }
      return context().set_qual_type(node, type::QualType::Error());
    } else {
    }
  }

  if (node_qual_types.size() != node->ids().size()) {
    return type::QualType::ErrorSpan();
  }

  size_t i = 0;
  for (auto const &id : node->ids()) {
    absl::Cleanup c = [&] { ++i; };
    if (id.name().empty()) {
      if (node_qual_types[i].type() == type::Module) {
        // TODO: check if it's constant?
        // TODO: check shadowing against other modules?
        // TODO: what if no init val is provded? what if not constant?

        if (auto maybe_mod =
                EvaluateOrDiagnoseAs<ir::ModuleId>(node->init_val())) {
          // TODO: In generic contexts it doesn't make sense to place this on
          // the AST.

          node->scope()->embed(
              &importer().get(*maybe_mod).as<CompiledModule>().scope());
        } else {
          node_qual_types[i].MarkError();
        }
      } else {
        diag().Consume(DeclaringHoleAsNonModule{
            .type  = node_qual_types[i].type(),
            .range = node->range(),
        });
        node_qual_types[i].MarkError();
      }

      return context().set_qual_types(&node->ids()[0], node_qual_types);
    }
  }

  // Gather all declarations with the same identifer that are visible in this
  // scope or that are in a scope which for which this declaration would be
  // visible. In other words, look both up and down the scope tree for
  // declarations of this identifier.
  //
  // It's tempting to assume we only need to look in one direction because we
  // would catch any ambiguity at a later time. However this is not correct.
  // For instance, consider this example:
  //
  // ```
  // if (cond) then {
  //   a := 4
  // }
  // a := 3  // Error: Redeclaration of `a`.
  // ```
  //
  // There is a redeclaration of `a` that needs to be caught. However, If we
  // only look towards the root of the scope tree, we will first see `a := 4`
  // which is not ambiguous. Later we will find `a := 3` which should have
  // been found but wasn't due to the fact that we saw the declaration that
  // was further from the root first while processing.
  //
  // The problem can be described mathematically as follows:
  //
  // Define *scope tree order* to be the partial order defined by D1 <= D2 iff
  // D1's path to the scope tree root is a prefix of D2's path to the scope
  // tree root. Define *processing order* to be the order in which nodes have
  // their types verified.
  //
  // The problem is that scope tree order does not have processing order as a
  // linear extension.
  //
  // To fix this particular problem, we need to make sure we check all
  // declarations that may be ambiguous regardless of whether they are above
  // or below `node` on the scope tree. However, we only want to look at the
  // ones which have been previously processed. This can be checked by looking
  // to see if we have saved the result of this declaration. We can also skip
  // out if the result was an error.

  i = 0;
  for (auto const &id : node->ids()) {
    absl::Cleanup c = [&] { ++i; };

    type::Typed<ast::Declaration::Id const *> typed_id(
        &id, node_qual_types[i].type());
    // TODO: struct field decls shouldn't have issues with shadowing local
    // variables.
    for (auto const *accessible_id :
         module::AllAccessibleDeclIds(node->scope(), id.name())) {
      if (&id == accessible_id) { continue; }
      auto qts = context().maybe_qual_type(accessible_id);
      if (not qts.data()) { continue; }
      if (Shadow(typed_id, type::Typed<ast::Declaration::Id const *>(
                               &id, qts[0].type()))) {
        // TODO: If one of these declarations shadows the other
        node_qual_types[i].MarkError();

        diag().Consume(ShadowingDeclaration{
            .range1 = node->range(),
            .range2 = id.range(),
        });
      }
    }
  }

  auto span = context().set_qual_types(node, node_qual_types);
  ASSERT(span.size() == node->ids().size());
  i = 0;
  for (auto const &id : node->ids()) {
    absl::Cleanup c = [&] { ++i; };
    context().set_qual_types(&id, absl::MakeConstSpan(&span[i], 1));
  }

  // TODO: verify special function signatures (copy, move, etc).
  return span;
}

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::Declaration::Id const *node) {
  LOG("Declaration::Id", "Verifying %s", node->name());
  return VerifyType(&node->declaration());
}

absl::Span<type::QualType const> Compiler::VerifyType(
    ast::BindingDeclaration const *node) {
  UNREACHABLE();
}

bool Compiler::VerifyPatternType(ast::Declaration const *node, type::Type t) {
  UNREACHABLE();
}

bool Compiler::VerifyPatternType(ast::BindingDeclaration const *node,
                                 type::Type t) {
  context().set_qual_type(node, type::QualType::Constant(t));
  // TODO: Do this explicitly on VerifyPatternType(Declaration::Id, type::Type)
  for (auto const &id : node->ids()) {
    context().set_qual_type(&id, type::QualType::Constant(t));
  }
  return true;
}

}  // namespace compiler
