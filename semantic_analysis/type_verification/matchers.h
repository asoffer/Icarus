#include <memory>

#include "compiler/context.h"
#include "diagnostic/consumer/tracking.h"
#include "frontend/parse.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/qual_type.h"

namespace semantic_analysis {

MATCHER_P(HasQualTypes, matcher, "") {
  ir::Module module(ir::ModuleId(1));
  compiler::Context ctx(&module);
  TypeVerifier tv(ctx);
  tv.schedule(std::addressof(arg));
  tv.complete();
  std::cerr << "=====\n";
  for (const auto& k : ctx.qual_types(std::addressof(arg))) {
    std::cerr << k << "\n";
  }
  std::cerr << "=====\n";
  return testing::ExplainMatchResult(
      matcher, ctx.qual_types(std::addressof(arg)), result_listener);
}

template <typename Node>
std::unique_ptr<Node> Parsed(std::string_view source) {
  diagnostic::TrackingConsumer consumer;
  auto stmts = frontend::Parse(source, consumer);
  if (consumer.num_consumed() != 0 or stmts.size() != 1) { return nullptr; }

  auto* ptr = stmts[0].release();
  if (auto* cast_ptr = ptr->if_as<Node>()) {
    return std::unique_ptr<Node>(cast_ptr);
  } else {
    delete cast_ptr;
    return nullptr;
  }
}

}  // namespace semantic_analysis
