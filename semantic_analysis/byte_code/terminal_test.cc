#include "ast/ast.h"
#include "ast/module.h"
#include "compiler/context.h"
#include "diagnostic/consumer/tracking.h"
#include "frontend/parse.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "jasmin/execute.h"
#include "semantic_analysis/byte_code/byte_code.h"
#include "semantic_analysis/type_verification/verify.h"

namespace semantic_analysis {
namespace {

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

IrFunction EmitByteCodeForSource(std::string_view source) {
  Infrastructure infra;
  auto nodes = infra.ParseAndVerify(source);
  return EmitByteCode(nodes.back()->as<ast::Expression>(), infra.context());
}

template <typename ReturnType>
ReturnType Evaluate(std::string_view source) {
  ReturnType result;
  IrFunction f = EmitByteCodeForSource(source);
  jasmin::Execute(f, {}, result);
  return result;
}

TEST(Terminal, Evaluation) {
  EXPECT_TRUE(Evaluate<bool>("true"));
  EXPECT_FALSE(Evaluate<bool>("false"));
}

}  // namespace
}  // namespace semantic_analysis

