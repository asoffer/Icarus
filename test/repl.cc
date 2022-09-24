#include "test/repl.h"

#include "ast/expression.h"
#include "compiler/context.h"
#include "frontend/parse.h"
#include "jasmin/execute.h"
#include "semantic_analysis/byte_code/byte_code.h"
#include "semantic_analysis/type_verification/verify.h"

namespace test {

Repl::ExecuteResult Repl::execute(std::string content) {
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());
  if (consumer_.num_consumed() != 0) {
    return ExecuteResult(source_content_.back());
  }

  semantic_analysis::TypeVerifier tv(context_, consumer_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  if (consumer_.num_consumed() != 0) {
    return ExecuteResult(source_content_.back());
  }

  auto const& expr = node_span.back()->as<ast::Expression>();
  semantic_analysis::IrFunction f =
      semantic_analysis::EmitByteCode(expr, context_, type_system_);
  return ExecuteResult(source_content_.back(), std::move(f));
}

Repl::TypeCheckResult Repl::type_check(std::string content) {
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());
  if (consumer_.num_consumed() != 0) {
    return TypeCheckResult(source_content_.back(), {semantic_analysis::Error()},
                           consumer_.diagnostics());
  }

  semantic_analysis::TypeVerifier tv(context_, consumer_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  return TypeCheckResult(
      source_content_.back(),
      context_.qualified_types(&node_span.back()->as<ast::Expression>()),
      consumer_.diagnostics());
}

}  // namespace test
