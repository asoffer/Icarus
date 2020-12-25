#include <string>
#include <utility>

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/library_module.h"
#include "diagnostic/message.h"
#include "ir/value/module_id.h"

namespace compiler {
namespace {

struct NonConstantImport {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "non-constant-import";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot import a non-constant module."),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  frontend::SourceRange range;
};

struct InvalidImport {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-import";

  diagnostic::DiagnosticMessage ToMessage(frontend::Source const *src) const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot import a module from a value of type `%s`.",
                         type),
        diagnostic::SourceQuote(src).Highlighted(range, diagnostic::Style{}));
  }

  type::Type type;
  frontend::SourceRange range;
};

}  // namespace

type::QualType Compiler::VerifyType(ast::Import const *node) {
  ASSIGN_OR(return _, auto result, VerifyType(node->operand()));

  auto qt  = type::QualType::Constant(type::Module);
  bool err = false;
  if (result.type() != type::ByteView) {
    diag().Consume(InvalidImport{.type  = result.type(),
                                 .range = node->operand()->range()});
    qt.MarkError();
    err = true;
  }

  if (not result.constant()) {
    diag().Consume(NonConstantImport{.range = node->operand()->range()});
    qt.MarkError();
    err = true;
  }

  if (err) { return context().set_qual_type(node, qt); }

  auto maybe_src = EvaluateAs<ir::String>(node->operand());
  if (not maybe_src) {
    diag().Consume(maybe_src.error());
    qt.MarkError();
    return context().set_qual_type(node, qt);
  }

  ir::ModuleId mod_id = importer().Import(maybe_src->get());
  if (mod_id == ir::ModuleId::Invalid()) {
    qt.MarkError();
  } else {
    context().set_imported_module(node, mod_id);
  }
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
