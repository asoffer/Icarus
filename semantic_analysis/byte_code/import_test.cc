#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

#if 0

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
  resources.module_map().insert(module::UniqueId::Self(),
                                module::UniqueId("blah"), id);
  return test::Repl(std::move(resources));
}

TEST(Import, Computation) {
  module::ModuleName name("abc");
  test::Repl repl = MakeRepl(name);

  module::UniqueId expected_index =
      repl.resources().TryLoadModuleByName(name);
  ASSERT_NE(expected_index, module::UniqueId::Invalid());

  EXPECT_EQ(repl.execute<module::UniqueId>(R"(import "abc")"),
            expected_index);
}

#endif

}  // namespace
}  // namespace semantic_analysis
