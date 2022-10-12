#include "test/repl.h"

#include "ast/expression.h"
#include "compiler/context.h"
#include "frontend/parse.h"
#include "jasmin/execute.h"
#include "semantic_analysis/byte_code/byte_code.h"
#include "semantic_analysis/type_verification/verify.h"

namespace test {

std::optional<semantic_analysis::IrFunction> Repl::ExecutionFunction(
    std::string&& content) {
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());
  if (consumer_.num_consumed() != 0) { return std::nullopt; }

  semantic_analysis::TypeVerifier tv(state_, context_, consumer_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  if (consumer_.num_consumed() != 0) { return std::nullopt; }

  auto const& expr = node_span.back()->as<ast::Expression>();
  return semantic_analysis::EmitByteCode(expr, context_, state_);
}

Repl::TypeCheckResult Repl::type_check(std::string content) {
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());
  if (consumer_.num_consumed() != 0) {
    return TypeCheckResult(source_content_.back(), {semantic_analysis::Error()},
                           consumer_.diagnostics(), *this);
  }

  semantic_analysis::TypeVerifier tv(state_, context_, consumer_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  return TypeCheckResult(
      source_content_.back(),
      context_.qualified_types(&node_span.back()->as<ast::Expression>()),
      consumer_.diagnostics(), *this);
}

}  // namespace test
