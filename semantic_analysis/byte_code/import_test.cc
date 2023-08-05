#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

test::Repl MakeRepl(module::ModuleName name) {
  auto resources = test::TestResources(
      [name = std::move(name)](module::ModuleName const &module_name) {
        if (module_name == name) {
          return module::UniqueId("module");
        } else {
          return module::UniqueId();
        }
      });

  module::UniqueId id("module");
  resources.AllocateModule(id);
  resources.module_map().insert(serialization::ModuleIndex::Self(),
                                serialization::ModuleIndex(0), id);
  return test::Repl(std::move(resources));
}

TEST(Import, Computation) {
  module::ModuleName name("abc");
  test::Repl repl = MakeRepl(name);

  serialization::ModuleIndex expected_index =
      repl.resources().TryLoadModuleByName(name);
  ASSERT_NE(expected_index, serialization::ModuleIndex::Invalid());

  EXPECT_EQ(repl.execute<serialization::ModuleIndex>(R"(import "abc")"),
            expected_index);
}

}  // namespace
}  // namespace semantic_analysis
