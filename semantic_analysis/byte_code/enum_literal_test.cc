#include "gtest/gtest.h"
#include "serialization/module_index.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::testing::_;
using ::testing::UnorderedElementsAre;
using ::testing::IsEmpty;
using ::testing::Pair;

TEST(EnumLiteral, Empty) {
  test::Repl repl;

  auto t          = repl.execute<core::Type>(R"(enum {})");
  auto const& enums = t.get<EnumType>(repl.type_system()).enumerators();
  EXPECT_THAT(enums, IsEmpty());
}

TEST(EnumLiteral, ElementsWithValues) {
  test::Repl repl;

  auto t = repl.execute<core::Type>(R"(enum { B ::= 5 \\ A ::= 1 })");
  auto const& enums = t.get<EnumType>(repl.type_system()).enumerators();
  EXPECT_THAT(enums, UnorderedElementsAre(Pair("A", 1), Pair("B", 5)));
}

TEST(EnumLiteral, ElementsWithSomeValues) {
  test::Repl repl;

  auto t            = repl.execute<core::Type>(R"(enum { A \\ B ::= 3 \\ C })");
  auto const& enums = t.get<EnumType>(repl.type_system()).enumerators();
  EXPECT_THAT(enums,
              UnorderedElementsAre(Pair("A", _), Pair("B", 3), Pair("C", _)));
}

TEST(EnumLiteral, ElementsWithNoValues) {
  test::Repl repl;

  auto t            = repl.execute<core::Type>(R"(enum { A \\ B \\ C })");
  auto const& enums = t.get<EnumType>(repl.type_system()).enumerators();
  EXPECT_THAT(enums,
              UnorderedElementsAre(Pair("A", _), Pair("B", _), Pair("C", _)));
}

}  // namespace
}  // namespace semantic_analysis
