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

ir::ModuleId Compiler::EvaluateModuleWithCache(ast::Expression const *expr) {
  // TODO: Implement caching behavior.
  if (auto maybe_mod = EvaluateOrDiagnoseAs<ir::ModuleId>(expr)) {
    return *maybe_mod;
  } else {
    return ir::ModuleId::Invalid();
  }
}

module::BasicModule const *ModuleFor(ast::Node const *node) {
  auto &scope     = *ASSERT_NOT_NULL(node->scope());
  auto &mod_scope = *ASSERT_NOT_NULL(scope.Containing<ast::ModuleScope>());
  return ASSERT_NOT_NULL(mod_scope.module());
}

frontend::SourceBuffer const *SourceBufferFor(ast::Node const *node) {
  return &ModuleFor(node)->buffer();
}

frontend::SourceView SourceViewFor(ast::Node const *node) {
  return frontend::SourceView(SourceBufferFor(node), node->range());
}

}  // namespace compiler
