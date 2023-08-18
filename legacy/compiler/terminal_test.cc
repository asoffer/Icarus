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

TEST(Terminal, BooleanPrimitives) {
  test::Snippet true_snippet(R"(true)");
  ASSERT_THAT(true_snippet,
              AllOf(HasQualTypes(Constant(Bool)), HasDiagnostics()));
  EXPECT_THAT(true_snippet, EvaluatesTo(true));

  test::Snippet false_snippet(R"(false)");
  ASSERT_THAT(false_snippet,
              AllOf(HasQualTypes(Constant(Bool)), HasDiagnostics()));
  EXPECT_THAT(false_snippet, EvaluatesTo(false));
}

TEST(Terminal, StringLiterals) {
  {
    test::Snippet s(R"("")");
    auto char_slice_type    = Constant(SliceType(GlobalTypeSystem, Char));
    EXPECT_THAT(s, AllOf(HasQualTypes(char_slice_type), HasDiagnostics()));
  }
  {
    test::Snippet s(R"("abc")");
    auto char_slice_type    = Constant(SliceType(GlobalTypeSystem, Char));
    EXPECT_THAT(s, AllOf(HasQualTypes(char_slice_type), HasDiagnostics()));
  }
  {
    test::Snippet s(R"("abc\"")");
    auto char_slice_type    = Constant(SliceType(GlobalTypeSystem, Char));
    EXPECT_THAT(s, AllOf(HasQualTypes(char_slice_type), HasDiagnostics()));
  }
  {
    test::Snippet s(R"("ab\n\r\t\v\\c")");
    auto char_slice_type    = Constant(SliceType(GlobalTypeSystem, Char));
    EXPECT_THAT(s, AllOf(HasQualTypes(char_slice_type), HasDiagnostics()));
  }
}

TEST(Terminal, Types) {
  {
    test::Snippet s(R"(byte)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(Byte));
  }

  {
    test::Snippet s(R"(bool)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(Bool));
  }

  {
    test::Snippet s(R"(u8)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(U(8)));
  }

  {
    test::Snippet s(R"(u16)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(U(16)));
  }

  {
    test::Snippet s(R"(u32)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(U(32)));
  }

  {
    test::Snippet s(R"(u64)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(U(64)));
  }

  {
    test::Snippet s(R"(i8)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(I(8)));
  }

  {
    test::Snippet s(R"(i16)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(I(16)));
  }

  {
    test::Snippet s(R"(i32)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(I(32)));
  }

  {
    test::Snippet s(R"(i64)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(I(64)));
  }

  {
    test::Snippet s(R"(f32)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(F32));
  }

  {
    test::Snippet s(R"(f64)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(F64));
  }

  {
    test::Snippet s(R"(integer)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(Integer));
  }

  {
    test::Snippet s(R"(type)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(Type));
  }

  {
    test::Snippet s(R"(module)");
    ASSERT_THAT(s, AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
    EXPECT_THAT(s, EvaluatesTo(Module));
  }
}

TEST(Terminal, Numbers) {
  test::Snippet n(R"(1234)");
  ASSERT_THAT(n, AllOf(HasQualTypes(Constant(Integer)), HasDiagnostics()));
  EXPECT_THAT(n, EvaluatesTo(absl::int128(1234)));

  test::Snippet r(R"(12.34)");
  ASSERT_THAT(r, AllOf(HasQualTypes(Constant(F64)), HasDiagnostics()));
  EXPECT_THAT(r, EvaluatesTo(12.34));
}

TEST(Terminal, Characters) {
  test::Snippet a(R"(!'a')");
  ASSERT_THAT(a, AllOf(HasQualTypes(Constant(Char)), HasDiagnostics()));
  EXPECT_THAT(a, EvaluatesTo('a'));

  test::Snippet b(R"(!'%')");
  ASSERT_THAT(b, AllOf(HasQualTypes(Constant(Char)), HasDiagnostics()));
  EXPECT_THAT(b, EvaluatesTo('%'));

  test::Snippet c(R"(!'\n')");
  ASSERT_THAT(c, AllOf(HasQualTypes(Constant(Char)), HasDiagnostics()));
  EXPECT_THAT(c, EvaluatesTo('\n'));
}

}  // namespace
}  // namespace compiler
