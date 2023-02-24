#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

test::Repl MakeRepl(module::ModuleName name) {
  auto resources = test::TestResources(
      [name = std::move(name)](module::ModuleName const &module_name) {
        if (module_name == name) {
          return serialization::UniqueModuleId("module");
        } else {
          return serialization::UniqueModuleId();
        }
      });
  serialization::Module m;
  m.set_identifier("module");
  resources.LoadFrom(std::move(m));
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
