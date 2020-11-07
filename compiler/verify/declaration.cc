#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/library_module.h"
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
        diagnostic::Text("Declaring `--` as non-module type `%s`.",
                         type.to_string()),
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
            .Line(range1.begin().line_num)
            .Line(range2.begin().line_num));
  }

  frontend::SourceRange range1;
  frontend::SourceRange range2;
};

struct NoDefaultValue {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "no-default-value";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("There is no default value for the type `%s`.",
                         type.to_string()),
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
  if (auto *tup = t.if_as<type::Tuple>()) {
    for (auto entry : tup->entries_) {
      auto reason = Inferrable(entry);
      if (reason != UninferrableType::Reason::kInferrable) { return reason; }
    }
  } else if (auto *f = t.if_as<type::Function>()) {
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
bool Shadow(type::Typed<ast::Declaration const *> decl1,
            type::Typed<ast::Declaration const *> decl2) {
  type::Callable const *callable1 = decl1.type().if_as<type::Callable>();
  type::Callable const *callable2 = decl2.type().if_as<type::Callable>();
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
                   auto type_expr_qt, compiler.VerifyType(node->type_expr()));
  if (type_expr_qt.type() != type::Type_) {
    compiler.diag().Consume(NotAType{
        .range = node->type_expr()->range(),
        .type  = type_expr_qt.type(),
    });
    return type::QualType::Error();
  }

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
type::QualType VerifyInferred(Compiler &compiler,
                              ast::Declaration const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto init_val_qt, compiler.VerifyType(node->init_val()));
  auto reason = Inferrable(init_val_qt.type());
  if (reason != UninferrableType::Reason::kInferrable) {
    compiler.diag().Consume(UninferrableType{
        .reason = reason,
        .range  = node->init_val()->range(),
    });
    return type::QualType::Error();
  }

  if (not internal::VerifyInitialization(compiler.diag(), node->range(),
                                         init_val_qt, init_val_qt)) {
    return type::QualType::Error();
  }

  type::Quals quals = (node->flags() & ast::Declaration::f_IsConst)
                          ? type::Quals::Const()
                          : type::Quals::Unqualified();
  return type::QualType(init_val_qt.type(), quals);
}

// Verifies the type of a declaration of the form `x: T = y`.
type::QualType VerifyCustom(Compiler &compiler, ast::Declaration const *node) {
  auto init_val_qt = compiler.VerifyType(node->init_val());

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

type::QualType Compiler::VerifyType(ast::Declaration const *node) {
  // Declarations can be seen out of order if they're constants and we happen to
  // verify an identifier referencing the declaration before the declaration is
  // processed in source-order.
  //
  // TODO: Consider first checking if it's a constant because we only need to do
  // this lookup in that case. Not sure how much performance that might win.
  if (auto const *qt = context().qual_type(node)) { return *qt; }
  LOG("Declaration", "Verifying %s", node->id());

  // TODO: If we don't already have type-checked this but it's an error, we'll
  // type-check this node again because we don't save errors. Maybe we should
  // revisit that idea. It's likely the cause of the problem where we generate
  // the same error message multiple times.

  type::QualType node_qual_type;
  switch (node->kind()) {
    case ast::Declaration::kDefaultInit: {
      ASSIGN_OR(return type::QualType::Error(),  //
                       node_qual_type,
                       VerifyDefaultInitialization(*this, node));
    } break;
    case ast::Declaration::kInferred: {
      ASSIGN_OR(return type::QualType::Error(),  //
                       node_qual_type, VerifyInferred(*this, node));
    } break;
    case ast::Declaration::kInferredAndUninitialized: {
      diag().Consume(UninferrableType{
          .reason = UninferrableType::Reason::kHole,
          .range  = node->init_val()->range(),
      });
      if (node->flags() & ast::Declaration::f_IsConst) {
        diag().Consume(UninitializedConstant{.range = node->range()});
      }
      return type::QualType::Error();
    } break;
    case ast::Declaration::kCustomInit: {
      ASSIGN_OR(return type::QualType::Error(),  //
                       node_qual_type, VerifyCustom(*this, node));
    } break;
    case ast::Declaration::kUninitialized: {
      ASSIGN_OR(return type::QualType::Error(),  //
                       node_qual_type, VerifyUninitialized(*this, node));
    } break;
    default: UNREACHABLE(node->DebugString());
  }

  context().set_qual_type(node, node_qual_type);

  if (node->id().empty()) {
    if (node_qual_type.type() == type::Module) {
      // TODO: check if it's constant?
      // TODO: check shadowing against other modules?
      // TODO: what if no init val is provded? what if not constant?

      auto maybe_mod = EvaluateAs<ir::ModuleId>(node->init_val());
      if (not maybe_mod) {
        diag().Consume(maybe_mod.error());
        node_qual_type.MarkError();
      } else {
        // TODO: In generic contexts it doesn't make sense to place this on the
        // AST.
        node->scope()->embedded_modules_.insert(
            maybe_mod->get<LibraryModule>());
      }
      return node_qual_type;
    } else {
      diag().Consume(DeclaringHoleAsNonModule{
          .type  = node_qual_type.type(),
          .range = node->range(),
      });
      node_qual_type.MarkError();
      return node_qual_type;
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

  type::Typed<ast::Declaration const *> typed_node_decl(node,
                                                        node_qual_type.type());
  // TODO: struct field decls shouldn't have issues with shadowing local
  // variables.
  for (auto const *decl :
       module::AllAccessibleDecls(node->scope(), node->id())) {
    if (decl == node) { continue; }
    ASSIGN_OR(continue, type::QualType q, qual_type_of(decl));
    if (Shadow(typed_node_decl,
               type::Typed<ast::Declaration const *>(decl, q.type()))) {
      diag().Consume(ShadowingDeclaration{
          .range1 = node->range(),
          .range2 = decl->range(),
      });
    }
  }

  // TODO: verify special function signatures (copy, move, etc).
  return node_qual_type;
}

}  // namespace compiler
