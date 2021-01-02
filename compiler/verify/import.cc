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
  if (result.type() != type::Slc(type::Char)) {
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

  auto maybe_src = Evaluate(type::Typed<ast::Expression const *>(
      node->operand(), type::Slc(type::Char)));

  if (not maybe_src) {
    diag().Consume(maybe_src.error());
    qt.MarkError();
    return context().set_qual_type(node, qt);
  }

  std::string name = [&] {
    auto handle = ir::ReadOnlyData.lock();
    return std::string(handle->raw(maybe_src->get<ir::Slice>().data().rodata()),
                       maybe_src->get<ir::Slice>().length());
  }();

  ir::ModuleId mod_id = importer().Import(name);
  if (mod_id == ir::ModuleId::Invalid()) {
    qt.MarkError();
  } else {
    context().set_imported_module(node, mod_id);
  }
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
