#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"

namespace compiler {
namespace {

using ::testing::_;
using ::testing::IsEmpty;
using ::testing::Pair;
using ::testing::Return;
using ::testing::UnorderedElementsAre;

TEST(Import, InvalidTypeBeingImported) {
  test::TestModule mod;
  EXPECT_CALL(mod.importer, Import).Times(0);
  auto const *import = mod.Append<ast::Expression>(R"(import 3)");
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-import")));
}

TEST(Import, NonConstantImport) {
  test::CompilerInfrastructure infra;
  EXPECT_CALL(infra.importer, Import).Times(0);
  auto &mod          = infra.add_module(R"(str := "abc")");
  auto const *import = mod.Append<ast::Expression>(R"(import str)");
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "non-constant-import")));
}

TEST(Import, NonConstantAndInvalidType) {
  test::CompilerInfrastructure infra;
  EXPECT_CALL(infra.importer, Import).Times(0);
  auto &mod = infra.add_module(R"(x := 3)");
  auto const *import = mod.Append<ast::Expression>(R"(import x)");
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(mod.consumer.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "invalid-import"),
                  Pair("value-category-error", "non-constant-import")));
}

TEST(Import, StringLiteral) {
  constexpr std::string_view kModule = "some-module";
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module("");
  EXPECT_CALL(infra.importer, Import(_, kModule))
      .WillOnce(Return(ir::ModuleId(7)));
  auto const *import = mod.Append<ast::Expression>(R"(import "some-module")");
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

TEST(Import, ConstantCharSlice) {
  constexpr std::string_view kModule = "some-module";
  test::CompilerInfrastructure infra;
  auto &mod = infra.add_module(R"(str ::= "some-module")");
  EXPECT_CALL(infra.importer, Import(_, kModule))
      .WillOnce(Return(ir::ModuleId(7)));
  auto const *import = mod.Append<ast::Expression>(R"(import str)");
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(mod.consumer.diagnostics(), IsEmpty());
}

}  // namespace
}  // namespace compiler
