#include "module/table.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "module/builtin.h"
#include "module/mock_module.h"

namespace module {
namespace {

using ::testing::Pair;
using ::testing::SizeIs;
using ::testing::UnorderedElementsAre;

TEST(ModuleTable, ConstructedWithBuiltin) {
  ModuleTable table(std::make_unique<BuiltinModule>());
  auto *module_by_numeric_id = table.module(ir::ModuleId::Builtin());
  ASSERT_NE(module_by_numeric_id, nullptr);
  EXPECT_TRUE(module_by_numeric_id->is<BuiltinModule>());

  auto [numeric_id, module_by_string_id] =
      table.module(BuiltinModule::BuiltinIdentifier);
  ASSERT_NE(module_by_string_id, nullptr);
  EXPECT_TRUE(module_by_string_id->is<BuiltinModule>());
  EXPECT_EQ(numeric_id, ir::ModuleId::Builtin());
  EXPECT_EQ(module_by_numeric_id, module_by_string_id);
}

TEST(ModuleTable, Insertion) {
  ModuleTable table(std::make_unique<BuiltinModule>());
  auto [id1, m1] = table.add_module<MockModule>("mock");
  auto [id2, m2] = table.add_module<MockModule>("mock");
  auto [id3, m3] = table.add_module<MockModule>("another_mock");
  EXPECT_EQ(id1, id2);
  EXPECT_EQ(m1, m2);
  EXPECT_NE(id1, id3);
  EXPECT_NE(m1, m3);
}

TEST(ModuleTable, Access) {
  ModuleTable table(std::make_unique<BuiltinModule>());
  auto [id1, m1] = table.add_module<MockModule>("mock");

  auto [id2, m2] = table.module("mock");
  ASSERT_EQ(id1, id2);
  ASSERT_EQ(m1, m2);

  EXPECT_EQ(m2, table.module(id2));
}

TEST(ModuleTable, Size) {
  ModuleTable table(std::make_unique<BuiltinModule>());
  EXPECT_THAT(table, SizeIs(1));
  table.add_module<MockModule>("mock1");
  EXPECT_THAT(table, SizeIs(2));
  table.add_module<MockModule>("mock2");
  EXPECT_THAT(table, SizeIs(3));
  table.add_module<MockModule>("mock1");
  EXPECT_THAT(table, SizeIs(3));
  table.add_module<MockModule>("mock3");
  EXPECT_THAT(table, SizeIs(4));
}

TEST(ModuleTable, Modules) {
  ModuleTable table(std::make_unique<BuiltinModule>());
  ir::ModuleId m1 = table.add_module<MockModule>("mock1").first;
  ir::ModuleId m2 = table.add_module<MockModule>("mock2").first;
  table.add_module<MockModule>("mock1");
  ir::ModuleId m3 = table.add_module<MockModule>("mock3").first;

  EXPECT_THAT(table.ids(),
              UnorderedElementsAre(Pair("~builtin", ir::ModuleId::Builtin()),
                                   Pair("mock1", m1), Pair("mock2", m2),
                                   Pair("mock3", m3)));
}

}  // namespace
}  // namespace module
