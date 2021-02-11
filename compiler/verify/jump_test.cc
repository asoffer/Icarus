#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

// TODO: Check that function body verification is scheduled.

TEST(Jump, StatelessSuccess) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(R"(
    jump() { goto b() }
  )"));

  ASSERT_TRUE(qts[0].type().is<type::Jump>());
  auto& j = qts[0].type().as<type::Jump>();
  EXPECT_FALSE(j.state().valid());
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Jump, StatefulSuccess) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(R"(
    jump [n: *i64] () { goto b() }
  )"));

  ASSERT_TRUE(qts[0].type().is<type::Jump>());
  auto& j = qts[0].type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::I64)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Jump, StateMustBeAPointer) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(R"(
    jump [n: i64] () { goto b() }
  )"));

  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-pointer-jump-state")));
}

TEST(Jump, StateMustNotBeABufferPointer) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(R"(
    jump [n: [*]i64] () { goto b() }
  )"));

  ASSERT_TRUE(qts[0].type().is<type::Jump>());
  auto& j = qts[0].type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::I64)));
  EXPECT_THAT(j.params(), IsEmpty());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "buffer-pointer-jump-state")));
}

TEST(Jump, StateMustBeNonConstant) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(R"(
    jump [n :: *i64] () { goto b() }
  )"));

  ASSERT_TRUE(qts[0].type().is<type::Jump>());
  auto& j = qts[0].type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::I64)));
  EXPECT_THAT(j.params(), IsEmpty());
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "constant-jump-state")));
}

TEST(Jump, InitialValue) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(R"(
    jump [p: *bool = null] () { goto b() }
  )"));

  ASSERT_TRUE(qts[0].type().is<type::Jump>());
  auto& j = qts[0].type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::Bool)));
  EXPECT_THAT(j.params(), IsEmpty());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "jump-state-initial-value")));
}

TEST(Jump, InitialValueInferred) {
  test::TestModule mod;
  mod.AppendCode(R"(
    ptr ::= null as *i64
    b ::= block {}
  )");
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(R"(
    jump [p := ptr] () { goto b() }
  )"));

  ASSERT_TRUE(qts[0].type().is<type::Jump>());
  auto& j = qts[0].type().as<type::Jump>();
  EXPECT_EQ(j.state(), type::Type(type::Ptr(type::I64)));
  EXPECT_THAT(j.params(), IsEmpty());
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "jump-state-initial-value")));
}

TEST(Jump, MultipleStateProblemsAllDiagnosed) {
  test::TestModule mod;
  mod.AppendCode(R"(b ::= block {}
  )");
  auto qts = mod.context().qual_types(mod.Append<ast::Expression>(R"(
    jump [n ::= 3] () { goto b() }
  )"));

  EXPECT_THAT(qts, UnorderedElementsAre(type::QualType::Error()));
  EXPECT_THAT(
      mod.consumer.diagnostics(),
      UnorderedElementsAre(Pair("type-error", "non-pointer-jump-state"),
                           Pair("type-error", "constant-jump-state"),
                           Pair("type-error", "jump-state-initial-value")));
}

}  // namespace
}  // namespace compiler
