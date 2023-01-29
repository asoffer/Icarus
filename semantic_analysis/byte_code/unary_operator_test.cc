#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

TEST(UnaryOperator, Pointers) {
  test::Repl repl;
  auto& type_system = repl.type_system();

  EXPECT_EQ(repl.execute<core::Type>("*bool"),
            core::PointerType(type_system, Bool));

  EXPECT_EQ(repl.execute<core::Type>("*i32"),
            core::PointerType(type_system, I(32)));

  EXPECT_EQ(repl.execute<core::Type>("***i32"),
            core::PointerType(
                type_system,
                core::PointerType(type_system,
                                  core::PointerType(type_system, I(32)))));
  EXPECT_EQ(repl.execute<core::Type>("[*]bool"),
            BufferPointerType(type_system, Bool));

  EXPECT_EQ(repl.execute<core::Type>("[*]i32"),
            BufferPointerType(type_system, I(32)));

  EXPECT_EQ(repl.execute<core::Type>("[*]*[*]i32"),
            BufferPointerType(
                type_system,
                core::PointerType(type_system,
                                  BufferPointerType(type_system, I(32)))));
}

TEST(UnaryOperator, Negation) {
  test::Repl repl;
  EXPECT_EQ(repl.execute<double>("-3.14"), -3.14);
  EXPECT_EQ(repl.execute<data_types::IntegerHandle>("-1234").value(),
            -nth::Integer(1234));
  EXPECT_EQ(repl.execute<data_types::IntegerHandle>("-(-1234)").value(),
            nth::Integer(1234));
}

}  // namespace
}  // namespace semantic_analysis
