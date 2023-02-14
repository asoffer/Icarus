#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "semantic_analysis/type_system.h"
#include "test/repl.h"

extern "C" {
int32_t MyFunction(int32_t n) { return n * n; }
}

namespace semantic_analysis {
namespace {

TEST(Call, BuiltinForeign) {
  test::Repl repl;

  auto result = repl.execute<IrFunction const *>(
      R"(builtin.foreign("MyFunction", i32 -> i32))");

  // Result needs to be computed before we look up `fn` in the foreign function
  // map, since it is populated during execution.
  EXPECT_EQ(reinterpret_cast<decltype(&MyFunction)>(
                repl.foreign_symbol_map().function_pointer(0)),
            &MyFunction);

  // TODO: Fix this. Provide access to the IrFunction*.
  // void (*fn)() = repl.foreign_symbol_map().function_pointer(0);
  // EXPECT_EQ(result, fn);
  //
  // data_types::IntegerTable table;
  // jasmin::ExecutionState<InstructionSet> state{table};
  //
  // int32_t value;
  // jasmin::Execute(*fn, state, {int32_t{7}}, value);
  // EXPECT_EQ(value, MyFunction(7));
}

TEST(Call, EvaluationWithoutArguments) {
  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= () -> () {}
      f()
      true
    )"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= () -> bool { return true }
      f()
    )"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= () -> bool { return true }
      'f
    )"));
  }
}

TEST(Call, EvaluationWithOneArgument) {
//   {
//     test::Repl repl;
//     EXPECT_TRUE(repl.execute<bool>(R"(
//       f ::= (b: bool) -> bool { return b }
//       f(true)
//     )"));
//   }

  // TODO: Enable
  // {
  //   test::Repl repl;
  //   EXPECT_TRUE(repl.execute<bool>(R"(
  //     f ::= (b: bool) -> bool { return true }
  //     f(b = true)
  //   )"));
  // }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= (b: bool) -> bool { return true }
      true'f
    )"));
  }

  // TODO: Enable
  // {
  //   test::Repl repl;
  //   EXPECT_TRUE(repl.execute<bool>(R"(
  //     f ::= (b: bool) -> bool { return true }
  //     (b = true)'f
  //   )"));
  // }
}

TEST(Call, EvaluationWithImplicitCast) {
  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= (n: i64) -> bool { return n < 10 as i64 }
      f(3)
    )"));
  }

  // TODO: Enable
  // {
  //   test::Repl repl;
  //   EXPECT_TRUE(repl.execute<bool>(R"(
  //     f ::= (n: i64) -> bool { return n < 10 as i64 }
  //     f(n = 3)
  //   )"));
  // }

  {
    test::Repl repl;
    EXPECT_FALSE(repl.execute<bool>(R"(
      f ::= (n: i64) -> bool { return n < 10 as i64 }
      10'f
    )"));
  }

  // TODO: Enable
  // {
  //   test::Repl repl;
  //   EXPECT_TRUE(repl.execute<bool>(R"(
  //     f ::= (n: i64) -> bool { return n < 10 as i64 }
  //     (n = 10)'f
  //   )"));
  // }
}

}  // namespace
}  // namespace semantic_analysis
