#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ::test::EvaluatesTo;

TEST(UnaryOperator, Pointers) {
  test::Repl repl;
  auto& type_system = repl.type_system();

  EXPECT_THAT(repl.execute("*bool"),
              EvaluatesTo(static_cast<core::Type>(
                  core::PointerType(type_system, Bool))));

  EXPECT_THAT(repl.execute("*i32"),
              EvaluatesTo(static_cast<core::Type>(
                  core::PointerType(type_system, I(32)))));

  EXPECT_THAT(repl.execute("***i32"),
              EvaluatesTo(static_cast<core::Type>(core::PointerType(
                  type_system,
                  core::PointerType(type_system,
                                    core::PointerType(type_system, I(32)))))));
  EXPECT_THAT(repl.execute("[*]bool"),
              EvaluatesTo(static_cast<core::Type>(
                  BufferPointerType(type_system, Bool))));

  EXPECT_THAT(repl.execute("[*]i32"),
              EvaluatesTo(static_cast<core::Type>(
                  BufferPointerType(type_system, I(32)))));

  EXPECT_THAT(repl.execute("[*]*[*]i32"),
              EvaluatesTo(static_cast<core::Type>(BufferPointerType(
                  type_system,
                  core::PointerType(type_system,
                                    BufferPointerType(type_system, I(32)))))));
}

TEST(UnaryOperator, Negation) {
  test::Repl repl;
  EXPECT_THAT(repl.execute("-3.14"), EvaluatesTo(-3.14));
}

}  // namespace
}  // namespace semantic_analysis
