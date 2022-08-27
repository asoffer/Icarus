#include <memory>

#include "compiler/context.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "type/qual_type.h"

namespace semantic_analysis {

MATCHER_P(HasQualTypes, matcher, "") {
  ir::Module module(ir::ModuleId(1));
  compiler::Context ctx(&module);
  TypeVerifier tv(ctx);
  tv(std::addressof(arg));
  return testing::ExplainMatchResult(
      matcher, ctx.qual_types(std::addressof(arg)), result_listener);
}

}  // namespace semantic_analysis
