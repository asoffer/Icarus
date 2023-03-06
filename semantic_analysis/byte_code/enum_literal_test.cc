#include "gtest/gtest.h"
#include "serialization/module_index.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::testing::ElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;

TEST(EnumLiteral, Empty) {
  test::Repl repl;

  auto t          = repl.execute<core::Type>(R"(enum {})");
  std::span enums = t.get<EnumType>(repl.type_system()).enumerators();
  EXPECT_THAT(enums, IsEmpty());
}

TEST(EnumLiteral, ElementsWithValues) {
  test::Repl repl;

  auto t          = repl.execute<core::Type>(R"(enum { B ::= 5 \\ A ::= 1 })");
  std::span enums = t.get<EnumType>(repl.type_system()).enumerators();
  std::vector e(enums.begin(), enums.end());
  EXPECT_THAT(e, ElementsAre(Pair("A", 1), Pair("B", 5)));
}

}  // namespace
}  // namespace semantic_analysis