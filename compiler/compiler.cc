#include "compiler/compiler.h"

#include "ast/ast.h"
#include "base/log.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "diagnostic/consumer/buffering.h"
#include "ir/interpreter/evaluate.h"

namespace compiler {

Compiler::Compiler(Context *context, PersistentResources const &resources)
    : context_(ASSERT_NOT_NULL(context)), resources_(resources) {}

Compiler::Compiler(Context *context, PersistentResources const &resources,
                   TransientState state)
    : context_(ASSERT_NOT_NULL(context)),
      resources_(resources),
      state_(std::move(state)) {}

std::optional<ir::CompleteResultBuffer> Compiler::EvaluateToBufferOrDiagnose(
    type::Typed<ast::Expression const *> expr) {
  auto maybe_result = EvaluateToBuffer(expr);
  if (auto *diagnostics = std::get_if<std::vector<diagnostic::ConsumedMessage>>(
          &maybe_result)) {
    for (auto &d : *diagnostics) { diag().Consume(std::move(d)); }
    return std::nullopt;
  } else {
    return std::get<ir::CompleteResultBuffer>(std::move(maybe_result));
  }
}

}  // namespace compiler
