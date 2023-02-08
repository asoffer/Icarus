#include "gtest/gtest.h"
#include "module/specified_module_map.h"
#include "serialization/module_map.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Import, Computation) {
  auto module_map = std::make_unique<module::SpecifiedModuleMap>();
  serialization::UniqueModuleId key("key");
  module::ModuleName name("abc");
  module_map->identify(name, key);
  test::Repl repl(std::move(module_map));

  auto const& mm = repl.module_map();
  auto id        = mm.id(name);
  ASSERT_TRUE(id);
  auto expected_id = serialization::ModuleIndex(mm.index(id.id()).value());

  EXPECT_EQ(repl.execute<serialization::ModuleIndex>(R"(import "abc")"), expected_id);
}

}  // namespace
}  // namespace semantic_analysis
