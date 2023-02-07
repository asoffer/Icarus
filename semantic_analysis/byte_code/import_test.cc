#include "gtest/gtest.h"
#include "module/specified_module_map.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(Import, Computation) {
  auto module_map = std::make_unique<module::SpecifiedModuleMap>();
  module::UniqueModuleId key("key");
  module::ModuleName name("abc");
  module_map->specify(name, key);
  module_map->emplace(key, static_cast<module::FilePath*>(nullptr),
                      static_cast<module::FilePath*>(nullptr));
  test::Repl repl(std::move(module_map));

  auto const& mm = repl.module().module_map();
  auto id        = mm.id(name);
  ASSERT_TRUE(id);
  auto expected_id = data_types::ModuleId(mm.index(id.id()).value());

  EXPECT_EQ(repl.execute<data_types::ModuleId>(R"(import "abc")"), expected_id);
}

}  // namespace
}  // namespace semantic_analysis
