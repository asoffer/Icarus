#include "compiler/compiler.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/repl.h"

namespace compiler {

namespace {

using ::test::EvaluatesTo;
using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;
using ::testing::Pair;

TEST(BufferPointer, Success) {
  test::Snippet snippet(R"([*]i64)");
  ASSERT_THAT(snippet, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  // EXPECT_THAT(snippet, EvaluatesTo(___));
}

TEST(BufferPointer, NonType) {
  test::Snippet snippet(R"([*]17)");
  ASSERT_THAT(snippet, AllOf(HasQualTypes(Error(Constant(Type))),
                             HasDiagnostics(Pair("type-error", "not-a-type"))));
}

}  // namespace
}  // namespace compiler
