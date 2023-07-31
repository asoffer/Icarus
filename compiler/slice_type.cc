#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {
namespace {

struct SliceDataTypeNotAType {
  static constexpr std::string_view kCategory = "type-error";
  static constexpr std::string_view kName     = "slice-data-type-not-a-type";

  diagnostic::DiagnosticMessage ToMessage() const {
    return diagnostic::DiagnosticMessage(
        diagnostic::Text(
            "Slice type has underlying data type specified as a value which "
            "is not a type."),
        diagnostic::SourceQuote().Highlighted(view, diagnostic::Style{}));
  }

  std::string_view view;
};

}  // namespace

// Verifies that the array type has constant integer lengths and that the data
// type expression is a type.
Compiler::Task Compiler::TaskFor(ast::SliceType const *node) {
  auto data_type_task = TaskFor(&node->data_type());
  auto data_qts       = data_type_task.get<std::vector<QualifiedType>>();

  bool error    = false;
  bool constant = true;

  if (data_qts.size() != 1) { NTH_UNIMPLEMENTED(); }
  if (data_qts[0].type() != Type) {
    ConsumeDiagnostic(SliceDataTypeNotAType{
        .view = node->data_type().range(),
    });
    error = true;
    co_return;
  } else if (not(data_qts[0].qualifiers() >= Qualifiers::Constant())) {
    constant = false;
  }

  co_yield std::vector{QualifiedType(
      Type, ((error ? Qualifiers::Error() : Qualifiers()) |
             (constant ? Qualifiers::Constant() : Qualifiers())))};

  auto data = co_await nth::type<FunctionData>;
  data_type_task.complete();
  switch (data.kind()) {
    case EmitKind::Value:
      data.function().AppendPush(&type_system());
      data.function().AppendMakeSliceType();
      break;
    case EmitKind::Statement: 

      break;
    default: NTH_UNREACHABLE();
  }
  co_return;
}

}  // namespace compiler
