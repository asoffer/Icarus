#include "compiler/common_diagnostics.h"

#include "compiler/common.h"
#include "compiler/context.h"

namespace compiler {

UncallableWithArguments UncallableError(
    Context const &context, ast::Expression const *name,
    absl::Span<ast::Call::Argument const> arguments,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>
        errors) {
  ASSERT(errors.size() != 0u);
  UncallableWithArguments result;
  result.view = SourceViewFor(name);

  size_t i = 0;
  for (; i < arguments.size(); ++i) {
    if (arguments[i].named()) { break; }
    result.arguments.pos_emplace(
        TypeForDiagnostic(&arguments[i].expr(), context));
  }
  for (; i < arguments.size(); ++i) {
    result.arguments.named_emplace(
        arguments[i].name(),
        TypeForDiagnostic(&arguments[i].expr(), context));
  }

  result.errors = std::move(errors);
  return result;
}

}  // namespace compiler
