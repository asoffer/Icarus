#include "ast/ast.h"
#include "compiler/common_diagnostics.h"
#include "semantic_analysis/type_system.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

struct NonConstantImport {
  static constexpr std::string_view kCategory = "value-category-error";
  static constexpr std::string_view kName     = "non-constant-import";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot import a non-constant module."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

struct InvalidImport {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "invalid-import";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("Cannot import a module from a value of type `%s`.",
                         type),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string type;
  std::string_view view;
};

struct NoSuchModule {
  static constexpr std::string_view kCategory = "value-error";
  static constexpr std::string_view kName     = "invalid-import";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text("No such module named \"%s\".", view),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

}  // namespace

VerificationTask TypeVerifier::VerifyType(TypeVerifier &tv,
                                          ast::Import const *node) {
  std::span arguments = co_await VerifyTypeOf(node->operand());
  ASSERT(arguments.size() == 1);

  auto qt = Constant(Module);
  if (arguments[0].type() != SliceType(tv.type_system(), Char)) {
    tv.ConsumeDiagnostic(
        InvalidImport{.type = tv.TypeForDiagnostic(*node->operand()),
                      .view = node->operand()->range()});
    qt = Error(qt);
  }

  if (not(arguments[0].qualifiers() >= Qualifiers::Constant())) {
    tv.ConsumeDiagnostic(NonConstantImport{.view = node->operand()->range()});
    qt = Error(qt);
  }

  if (not(qt.qualifiers() >= Qualifiers::Error())) {
    std::span name_bytes = tv.EvaluateConstant(node->operand(), arguments[0]);
    char const *ptr;
    size_t length;
    auto *p = name_bytes.data();
    std::memcpy(&ptr, p, sizeof(ptr));
    std::memcpy(&length, p + jasmin::ValueSize, sizeof(length));
    std::string_view name(ptr, length);

    auto index = tv.resources().TryLoadModuleByName(module::ModuleName(name));
    if (index == serialization::ModuleIndex::Invalid()) {
      tv.ConsumeDiagnostic(NoSuchModule{.view = node->operand()->range()});
      qt = Error(qt);
    } else {
      auto [ptr, inserted] = tv.context().insert_constant(node);
      ASSERT(inserted);
      ptr->resize(sizeof(index));
      std::memcpy(ptr->data(), &index, sizeof(index));
    }
  }

  co_return tv.TypeOf(node, qt);
}

}  // namespace semantic_analysis