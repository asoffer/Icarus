#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/overload_set.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(UnconditionalGoto, Success) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {
    after ::= jump () { goto b() }
  }
  )");
  ASSERT_THAT(mod.consumer.diagnostics(), IsEmpty()) << "Test setup failed.";

  mod.AppendCode("goto b()");
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ConditionalGoto, Success) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {
    after ::= jump () { goto b() }
  }
  )");
  ASSERT_THAT(mod.consumer.diagnostics(), IsEmpty()) << "Test setup failed.";

  mod.AppendCode("goto true, b(), b()");
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(ConditionalGoto, NonBooleanConditionalType) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {
    after ::= jump () { goto b() }
  }
  )");
  ASSERT_THAT(mod.consumer.diagnostics(), IsEmpty()) << "Test setup failed.";

  mod.AppendCode("goto 3, b(), b()");
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-boolean-conditional-goto")));
}

// TODO: Add tests where there are type verification issues in the jumps.

}  // namespace
}  // namespace compiler
