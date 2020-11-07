#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::Pointee;
using ::testing::UnorderedElementsAre;

// TODO: Check that function body verification is scheduled.

TEST(Jump, StatelessSuccess) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto const* qt = mod.context().qual_type(mod.Append<ast::Expression>(R"(
    jump() { goto b() }
  )"));

  ASSERT_NE(qt, nullptr);
  ASSERT_TRUE(qt->type().is<type::Jump>());
  auto& j = qt->type().as<type::Jump>();
  EXPECT_FALSE(j.state().valid());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Jump, StatefulSuccess) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto const* qt = mod.context().qual_type(mod.Append<ast::Expression>(R"(
    jump [n: *int64] () { goto b() }
  )"));

  ASSERT_NE(qt, nullptr);
  ASSERT_TRUE(qt->type().is<type::Jump>());
  auto& j = qt->type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::Int64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Jump, StateMustBeAPointer) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto const* qt = mod.context().qual_type(mod.Append<ast::Expression>(R"(
    jump [n: int64] () { goto b() }
  )"));

  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-pointer-jump-state")));
}

TEST(Jump, StateMustNotBeABufferPointer) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto const* qt = mod.context().qual_type(mod.Append<ast::Expression>(R"(
    jump [n: [*]int64] () { goto b() }
  )"));

  ASSERT_NE(qt, nullptr);
  ASSERT_TRUE(qt->type().is<type::Jump>());
  auto& j = qt->type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::Int64)));
  EXPECT_THAT(j.params(), IsEmpty());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "buffer-pointer-jump-state")));
}

TEST(Jump, StateMustBeNonConstant) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto const* qt = mod.context().qual_type(mod.Append<ast::Expression>(R"(
    jump [n :: *int64] () { goto b() }
  )"));

  ASSERT_NE(qt, nullptr);
  ASSERT_TRUE(qt->type().is<type::Jump>());
  auto& j = qt->type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::Int64)));
  EXPECT_THAT(j.params(), IsEmpty());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "constant-jump-state")));
}

TEST(Jump, InitialValue) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto const* qt = mod.context().qual_type(mod.Append<ast::Expression>(R"(
    jump [p: *bool = null] () { goto b() }
  )"));

  ASSERT_NE(qt, nullptr);
  ASSERT_TRUE(qt->type().is<type::Jump>());
  auto& j = qt->type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::Bool)));
  EXPECT_THAT(j.params(), IsEmpty());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "jump-state-initial-value")));
}

TEST(Jump, InitialValueInferred) {
  test::TestModule mod;
  mod.AppendCode(R"(
    b ::= block {}
    n := 3
  )");
  auto const* qt = mod.context().qual_type(mod.Append<ast::Expression>(R"(
    jump [p := &n] () { goto b() }
  )"));

  ASSERT_NE(qt, nullptr);
  ASSERT_TRUE(qt->type().is<type::Jump>());
  auto& j = qt->type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::Int64)));
  EXPECT_THAT(j.params(), IsEmpty());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "jump-state-initial-value")));
}

TEST(Jump, MultipleStateProblemsAllDiagnosed) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto const* qt = mod.context().qual_type(mod.Append<ast::Expression>(R"(
    jump [n ::= 3] () { goto b() }
  )"));

  ASSERT_EQ(qt, nullptr);
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-pointer-jump-state"),
                           Pair("type-error", "constant-jump-state"),
                           Pair("type-error", "jump-state-initial-value")));
}

}  // namespace
}  // namespace compiler
