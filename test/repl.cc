#include "test/repl.h"

#include "ast/expression.h"
#include "frontend/parse.h"
#include "jasmin/execute.h"
#include "semantic_analysis/byte_code/byte_code.h"
#include "semantic_analysis/type_verification/verify.h"

namespace test {

std::optional<semantic_analysis::IrFunction> Repl::ExecutionFunction(
    std::string&& content) {
  consumer_.clear();
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());
  if (consumer_.num_consumed() != 0) { return std::nullopt; }

  semantic_analysis::TypeVerifier tv(module_, context_, consumer_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  if (consumer_.num_consumed() != 0) { return std::nullopt; }

  auto const& expr = node_span.back()->as<ast::Expression>();
  return semantic_analysis::EmitByteCode(context_.qualified_type(&expr), expr,
                                         context_, module_);
}

Repl::TypeCheckResult Repl::type_check(std::string content) {
  size_t previously_consumed = consumer_.num_consumed();
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());

  semantic_analysis::QualifiedType error = semantic_analysis::Error();
  if (consumer_.num_consumed() != previously_consumed) {
    return TypeCheckResult(source_content_.back(), std::span(&error, 1),
                           consumer_.diagnostics(), *this);
  }

  consumer_.clear();

  semantic_analysis::TypeVerifier tv(module_, context_, consumer_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  auto* e = node_span.back()->if_as<ast::Expression>();
  return TypeCheckResult(source_content_.back(),
                         e ? context_.qualified_types(e)
                           : std::vector<semantic_analysis::QualifiedType>{},
                         consumer_.diagnostics(), *this);
}

}  // namespace test
