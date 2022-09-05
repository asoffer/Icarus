#include <memory>

#include "compiler/context.h"
#include "diagnostic/consumer/tracking.h"
#include "frontend/parse.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/qual_type.h"

// TODO: Enable nicer printing.
namespace ast {

std::ostream& operator<<(std::ostream& os, ast::Node const& node) {
  return os << node.DebugString();
}

}  // namespace ast

namespace semantic_analysis {

MATCHER_P(HasQualTypes, matcher, "") {
  ir::Module module(ir::ModuleId(1));
  compiler::Context ctx(&module);
  diagnostic::TrackingConsumer consumer;
  TypeVerifier tv(ctx, consumer);
  tv.schedule(std::addressof(arg));
  tv.complete();

  return testing::ExplainMatchResult(
      matcher, ctx.qual_types(std::addressof(arg)), result_listener);
}

struct Infrastructure {
  Infrastructure() {
    context_.set_qt_callback(
        [](ast::Declaration::Id const*, type::QualType) {});
  }

  base::PtrSpan<ast::Node const> ParseAndVerify(std::string_view source) {
    auto nodes              = frontend::Parse(source, consumer_);
    base::PtrSpan node_span = ast_module_.insert(nodes.begin(), nodes.end());

    TypeVerifier tv(context_, consumer_);
    for (auto const* node : node_span) { tv.schedule(node); }
    tv.complete();

    return node_span;
  }

  compiler::Context const& context() const { return context_; }

  absl::Span<std::pair<std::string, std::string> const> diagnostics() const {
    return consumer_.diagnostics();
  }

 private:
  ast::Module ast_module_{nullptr};
  ir::Module module_         = ir::Module(ir::ModuleId(1));
  compiler::Context context_ = compiler::Context(&module_);
  diagnostic::TrackingConsumer consumer_;
};

}  // namespace semantic_analysis
