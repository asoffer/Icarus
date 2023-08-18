#include "test/repl.h"

#include "ast/expression.h"
#include "frontend/parse.h"
#include "jasmin/execute.h"
#include "semantic_analysis/byte_code/byte_code.h"
#include "semantic_analysis/type_verification/verify.h"

namespace test {

std::optional<vm::Function> Repl::ExecutionFunction(std::string&& content) {
  consumer_->clear();
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), *consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());
  if (consumer_->num_consumed() != 0) { return std::nullopt; }

  semantic_analysis::TypeVerifier tv(resources(), context_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  if (consumer_->num_consumed() != 0) { return std::nullopt; }

  auto const& expr = node_span.back()->as<ast::Expression>();
  return semantic_analysis::EmitByteCode(context_.qualified_type(&expr), expr,
                                         context_, resources());
}

Repl::TypeCheckResult Repl::type_check(std::string content) {
  size_t previously_consumed = consumer_->num_consumed();
  source_content_.push_back(std::move(content));
  auto nodes              = frontend::Parse(source_content_.back(), *consumer_);
  base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());

  semantic_analysis::QualifiedType error = semantic_analysis::Error();
  if (consumer_->num_consumed() != previously_consumed) {
    return TypeCheckResult(source_content_.back(), std::span(&error, 1),
                           consumer_->diagnostics(), *this);
  }

  consumer_->clear();

  semantic_analysis::TypeVerifier tv(resources(), context_);
  for (auto const* node : node_span) { tv.schedule(node); }
  tv.complete();

  auto* e = node_span.back()->if_as<ast::Expression>();
  return TypeCheckResult(source_content_.back(),
                         e ? context_.qualified_types(e)
                           : std::vector<semantic_analysis::QualifiedType>{},
                         consumer_->diagnostics(), *this);
}

Snippet::Snippet(std::string content, module::Resources resources)
    : resources_(std::move(resources)),
      content_(std::move(content)),
      consumer_(static_cast<diagnostic::TrackingConsumer&>(
          resources_.diagnostic_consumer())) {
  size_t previously_consumed = consumer_.num_consumed();
  auto nodes                 = frontend::Parse(content_, consumer_);
  base::PtrSpan node_span    = ast_module_.insert(nodes.begin(), nodes.end());

  if (consumer_.num_consumed() != previously_consumed) {
    qualified_types_ = {semantic_analysis::Error()};
    return;
  }

  compiler::Compiler compiler(this->resources(), context_);

  for (auto const* node : node_span) {
    auto task = compiler(node);
    qualified_types_ =
        task.get<std::vector<semantic_analysis::QualifiedType>>();
  }
}

vm::Function Snippet::ExecutionFunction() const {
  compiler::Compiler compiler(this->resources(), context_);

  core::TypeContour contour =
      semantic_analysis::ContourOf(qualified_types_[0].type());
  size_t values_needed = contour.bytes().value() / jasmin::ValueSize +
                         (((contour.bytes().value() % jasmin::ValueSize) != 0));
  vm::Function f(0, values_needed);

  // This `variable_offsets` map is intentionally empty. There will never be
  // declarations from which data needs to be loaded. Because `EvaluateConstant`
  // is only to be called on constant expressions, any identifier will refer to
  // a declaration that is constant, and so lookup will happen by loading the
  // value directly rather than adding instructions which load at runtime.
  nth::flyweight_map<ast::Declaration::Id const*, size_t> variable_offsets;

  semantic_analysis::FunctionData data(f, variable_offsets);
  semantic_analysis::TemporarilySet temp(data.kind(),
                                         semantic_analysis::EmitKind::Value);
  for (auto const* node : ast_module_.stmts()) {
    auto task = compiler(node);
    task.get<std::vector<semantic_analysis::QualifiedType>>();
    task.send<semantic_analysis::FunctionData>(data);
    task.complete();
  }
  f.AppendReturn();
  return f;
}

}  // namespace test
