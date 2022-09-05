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
  std::vector<std::unique_ptr<ast::Node>> ParseAndVerify(
      std::string_view source) {
    auto nodes = frontend::Parse(source, consumer_);

    TypeVerifier tv(context_, consumer_);
    for (auto const& node : nodes) { tv.schedule(node.get()); }
    tv.complete();

    return nodes;
  }

  compiler::Context const& context() const { return context_; }

  absl::Span<std::pair<std::string, std::string> const> diagnostics() const {
    return consumer_.diagnostics();
  }

 private:
  ir::Module module_         = ir::Module(ir::ModuleId(1));
  compiler::Context context_ = compiler::Context(&module_);
  diagnostic::TrackingConsumer consumer_;
};

}  // namespace semantic_analysis
