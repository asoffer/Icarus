#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "serialization/module_map.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;
using ::testing::Pair;

test::Repl MakeRepl(std::optional<std::string> name = std::nullopt) {
  module::Resources resources(
      [name = std::move(name)](module::ModuleName const &module_name) {
        if (name and module_name.name() == *name) {
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

TEST(Import, Success) {
  test::Repl repl = MakeRepl("abc");
  EXPECT_THAT(repl.type_check(R"(import "abc")"),
              AllOf(HasQualTypes(Constant(Module)), HasDiagnostics()));
}

TEST(Import, NonExistantModule) {
  test::Repl repl = MakeRepl();
  EXPECT_THAT(repl.type_check(R"(import "abc")"),
              AllOf(HasQualTypes(Error(Constant(Module))),
                    HasDiagnostics(Pair("value-error", "invalid-import"))));
}

TEST(Import, NonConstantSlice) {
  test::Repl repl = MakeRepl();
  EXPECT_THAT(repl.type_check(R"(
  s := "abc"
  import s
  )"),
              AllOf(HasQualTypes(Error(Constant(Module))),
                    HasDiagnostics(
                        Pair("value-category-error", "non-constant-import"))));
}

TEST(Import, InvalidArgumentType) {
  test::Repl repl = MakeRepl();
  EXPECT_THAT(repl.type_check(R"(
  import 1234
  )"),
              AllOf(HasQualTypes(Error(Constant(Module))),
                    HasDiagnostics(Pair("type-error", "invalid-import"))));
}

TEST(Import, InvalidAndNonConstant) {
  test::Repl repl = MakeRepl();
  EXPECT_THAT(
      repl.type_check(R"(
  b: bool
  import b 
  )"),
      AllOf(HasQualTypes(Error(Constant(Module))),
            HasDiagnostics(Pair("value-category-error", "non-constant-import"),
                           Pair("type-error", "invalid-import"))));
}

}  // namespace
}  // namespace semantic_analysis
