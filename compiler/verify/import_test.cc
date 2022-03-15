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
  auto importer = std::make_unique<module::MockImporter>();
  EXPECT_CALL(*importer, Import).Times(0);
  test::CompilerInfrastructure infra(std::move(importer));
  auto &mod          = infra.add_module("import 3");
  auto const *import = mod.get<ast::Expression>();
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(Pair("type-error", "invalid-import")));
}

TEST(Import, NonConstantImport) {
  auto importer = std::make_unique<module::MockImporter>();
  EXPECT_CALL(*importer, Import).Times(0);
  test::CompilerInfrastructure infra(std::move(importer));
  auto &mod          = infra.add_module(R"(
  str := "abc"
  import str
  )");
  auto const *import = mod.get<ast::Expression>();
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("value-category-error", "non-constant-import")));
}

TEST(Import, NonConstantAndInvalidType) {
  auto importer = std::make_unique<module::MockImporter>();
  EXPECT_CALL(*importer, Import).Times(0);
  test::CompilerInfrastructure infra(std::move(importer));
  auto &mod          = infra.add_module(R"(
  x := 3
  import x
  )");
  auto const *import = mod.get<ast::Expression>();
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(infra.diagnostics(),
              UnorderedElementsAre(
                  Pair("type-error", "invalid-import"),
                  Pair("value-category-error", "non-constant-import")));
}

TEST(Import, StringLiteral) {
  constexpr std::string_view kModule = "some-module";
  auto importer                      = std::make_unique<module::MockImporter>();
  EXPECT_CALL(*importer, Import(_, kModule)).WillOnce(Return(ir::ModuleId(7)));
  test::CompilerInfrastructure infra(std::move(importer));
  auto &mod          = infra.add_module(R"(import "some-module")");
  auto const *import = mod.get<ast::Expression>();
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

TEST(Import, ConstantCharSlice) {
  constexpr std::string_view kModule = "some-module";
  auto importer                      = std::make_unique<module::MockImporter>();
  EXPECT_CALL(*importer, Import(_, kModule)).WillOnce(Return(ir::ModuleId(7)));
  test::CompilerInfrastructure infra(std::move(importer));
  auto &mod          = infra.add_module(R"(
  str ::= "some-module"
  import str
  )");
  auto const *import = mod.get<ast::Expression>();
  auto qts           = mod.context().qual_types(import);
  EXPECT_THAT(qts,
              UnorderedElementsAre(type::QualType::Constant(type::Module)));
  EXPECT_THAT(infra.diagnostics(), IsEmpty());
}

}  // namespace
}  // namespace compiler
