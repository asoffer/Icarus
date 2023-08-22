#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_verification/verify.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::HasDiagnostics;
using ::test::HasQualTypes;
using ::testing::AllOf;

TEST(Terminal, BooleanPrimitives) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(true)"),
              AllOf(HasQualTypes(Constant(Bool)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(false)"),
              AllOf(HasQualTypes(Constant(Bool)), HasDiagnostics()));
}

TEST(Terminal, StringLiterals) {
  test::Repl repl;
  QualifiedType const char_slice_type =
      Constant(SliceType(GlobalTypeSystem, Char));
  EXPECT_THAT(repl.type_check(R"("")"),
              AllOf(HasQualTypes(Constant(char_slice_type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"("abc")"),
              AllOf(HasQualTypes(Constant(char_slice_type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"("abc\"")"),
              AllOf(HasQualTypes(Constant(char_slice_type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"("ab\n\r\t\v\\c")"),
              AllOf(HasQualTypes(Constant(char_slice_type)), HasDiagnostics()));
}

TEST(Terminal, Types) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(byte)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(bool)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(u8)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(u16)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(u32)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(u64)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(i8)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(i16)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(i32)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(i64)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(f32)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(f64)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(integer)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(type)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(module)"),
              AllOf(HasQualTypes(Constant(Type)), HasDiagnostics()));
}

TEST(Terminal, Numbers) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(1234)"),
              AllOf(HasQualTypes(Constant(Integer)), HasDiagnostics()));
  EXPECT_THAT(repl.type_check(R"(12.34)"),
              AllOf(HasQualTypes(Constant(F64)), HasDiagnostics()));
}

TEST(Terminal, Characters) {
  test::Repl repl;
  EXPECT_THAT(repl.type_check(R"(!'a')"),
              AllOf(HasQualTypes(Constant(Char)), HasDiagnostics()));
}

}  // namespace
}  // namespace semantic_analysis