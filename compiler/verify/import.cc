#include <string>
#include <utility>

#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/compiler.h"
#include "diagnostic/message.h"
#include "ir/value/module_id.h"

namespace compiler {
namespace {

struct NonConstantImport {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "non-constant-import";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot import a non-constant module."),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  frontend::SourceView view;
};

struct InvalidImport {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-import";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot import a module from a value of type `%s`.",
                         type),
        diagnostic::SourceQuote(&view.buffer())
            .Highlighted(view.range(), diagnostic::Style{}));
  }

  type::Type type;
  frontend::SourceView view;
};

}  // namespace

absl::Span<type::QualType const> Compiler::VerifyType(ast::Import const *node) {
  LOG("Import", "%s", node->DebugString());
  ASSIGN_OR(return context().set_qual_type(node, _),  //
                   auto result, VerifyType(node->operand())[0]);

  auto qt  = type::QualType::Constant(type::Module);
  bool err = false;
  if (result.type() != type::Slc(type::Char)) {
    diag().Consume(InvalidImport{.type = result.type(),
                                 .view = SourceViewFor(node->operand())});
    err = true;
  }

  if (not result.constant()) {
    diag().Consume(NonConstantImport{.view = SourceViewFor(node->operand())});
    err = true;
  }

  if (err) {
    qt.MarkError();
    return context().set_qual_type(node, qt);
  }

  auto source_locator =
      EvaluateToBufferOrDiagnose(type::Typed<ast::Expression const *>(
          node->operand(), type::Slc(type::Char)));
  if (not source_locator) {
    qt.MarkError();
    return context().set_qual_type(node, qt);
  }

  auto slice          = source_locator->get<ir::Slice>(0);
  ir::ModuleId mod_id = importer().Import(slice);
  if (mod_id == ir::ModuleId::Invalid()) {
    qt.MarkError();
  } else {
    context().set_imported_module(node, mod_id);
  }
  return context().set_qual_type(node, qt);
}

}  // namespace compiler
