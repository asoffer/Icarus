#include "compiler/compiler.h"
#include "compiler/module.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::ElementsAre;
using ::testing::Eq;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::SizeIs;
using ::testing::UnorderedElementsAre;

TEST(InterfaceLiteral, Trivial) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(interface [T] {})");
  auto const *intf = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(intf);
  ASSERT_THAT(qts, ElementsAre(type::QualType::Constant(type::Interface)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(InterfaceLiteral, Simple) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(interface [T] {
    a :: builtin.callable()
  })");
  auto const *intf = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(intf);
  ASSERT_THAT(qts, ElementsAre(type::QualType::Constant(type::Interface)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(InterfaceLiteral, NonInterfaceMember) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(interface [T] {
    a :: 7
  })");
  auto const *intf = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(intf);
  ASSERT_THAT(qts, ElementsAre(type::QualType::Constant(type::Interface)));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "non-interface-member")));
}

TEST(InterfaceLiteral, SuccessWithJustType) {
  test::CompilerInfrastructure infra;
  auto &mod        = infra.add_module(R"(interface [T] {
    a :: T
  })");
  auto const *intf = mod.get<ast::Expression>();
  auto qts         = mod.context().qual_types(intf);
  ASSERT_THAT(qts, ElementsAre(type::QualType::Constant(type::Interface)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

}  // namespace
}  // namespace compiler
