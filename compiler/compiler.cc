#include "compiler/compiler.h"

#include "ast/ast.h"
#include "base/log.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/instructions.h"
#include "diagnostic/consumer/buffering.h"
#include "ir/compiled_fn.h"
#include "ir/compiled_jump.h"
#include "ir/interpreter/evaluate.h"
#include "type/generic_struct.h"
#include "type/jump.h"

namespace compiler {

Compiler::Compiler(Context *context, PersistentResources const &resources)
    : context_(ASSERT_NOT_NULL(context)), resources_(resources) {}

Compiler::Compiler(Context *context, PersistentResources const &resources,
                   TransientState state)
    : context_(ASSERT_NOT_NULL(context)),
      resources_(resources),
      state_(std::move(state)) {}

interpreter::EvaluationResult Compiler::Evaluate(
    type::Typed<ast::Expression const *> expr, bool must_complete) {
  auto maybe_result = EvaluateToBuffer(expr);
  if (auto *diagnostics = std::get_if<std::vector<diagnostic::ConsumedMessage>>(
          &maybe_result)) {
    for (auto &d : *diagnostics) { diag().Consume(std::move(d)); }
    return interpreter::EvaluationResult{interpreter::EvaluationResult::Failure{
        .failure = interpreter::EvaluationResult::Failure::Reason ::Unknown,
        .range   = (*expr)->range(),
    }};
  } else {
    return std::get<ir::CompleteResultBuffer>(std::move(maybe_result));
  }
}

std::optional<ir::CompleteResultBuffer> Compiler::EvaluateToBufferOrDiagnose(
    type::Typed<ast::Expression const *> expr, bool must_complete) {
  auto maybe_result = EvaluateToBuffer(expr);
  if (auto *diagnostics = std::get_if<std::vector<diagnostic::ConsumedMessage>>(
          &maybe_result)) {
    for (auto &d : *diagnostics) { diag().Consume(std::move(d)); }
    return std::nullopt;
  } else {
    return std::get<ir::CompleteResultBuffer>(std::move(maybe_result));
  }
}

ir::ModuleId Compiler::EvaluateModuleWithCache(ast::Expression const *expr) {
  // TODO: Implement caching behavior.
  if (auto maybe_mod = EvaluateOrDiagnoseAs<ir::ModuleId>(expr)) {
    return *maybe_mod;
  } else {
    return ir::ModuleId::Invalid();
  }
}

}  // namespace compiler
