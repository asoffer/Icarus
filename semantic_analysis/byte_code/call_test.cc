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
                repl.foreign_symbol_map().function(0)),
            &MyFunction);

  // TODO: Fix this. Provide access to the IrFunction*.
  // void (*fn)() = repl.foreign_symbol_map().function(0);
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
  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= (b: bool) -> bool { return b }
      f(true)
    )"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= (b: bool) -> bool { return b }
      f(b = true)
    )"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= (b: bool) -> bool { return b }
      true'f
    )"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= (b: bool) -> bool { return b }
      (b = true)'f
    )"));
  }
}

TEST(Call, EvaluationWithImplicitCast) {
  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= (n: i64) -> bool { return n < 10 as i64 }
      f(3)
    )"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"(
      f ::= (n: i64) -> bool { return n < 10 as i64 }
      f(n = 3)
    )"));
  }

  {
    test::Repl repl;
    EXPECT_FALSE(repl.execute<bool>(R"(
      f ::= (n: i64) -> bool { return n < 10 as i64 }
      10'f
    )"));
  }

  {
    test::Repl repl;
    EXPECT_FALSE(repl.execute<bool>(R"(
      f ::= (n: i64) -> bool { return n < 10 as i64 }
      (n = 10)'f
    )"));
  }
}

TEST(Call, MultipleArguments) {
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      f(3, 4)
    )"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      f(3, n = 4)
    )"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      f(m = 3, n = 4)
    )"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      f(n = 3, m = 4)
    )"),
              13);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      3'f(4)
    )"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      3'f(n = 4)
    )"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      (m = 3)'f(n = 4)
    )"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      (n = 3)'f(m = 4)
    )"),
              13);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      (3, 4)'f
    )"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      (3, n = 4)'f
    )"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      (m = 3, n = 4)'f
    )"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"(
      f ::= (m: i64, n: i64) -> i64 { return m + n * n }
      (n = 3, m = 4)'f
    )"),
              13);
  }
}

TEST(Call, EvaluationWithoutArgumentsNonConstant) {
  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"((() -> {
      f := () -> () {}
      f()
      return true
    })())"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"((() -> {
      f := () -> bool { return true }
      return f()
    })())"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"((() -> {
      f := () -> bool { return true }
      return 'f
    })())"));
  }
}

TEST(Call, EvaluationWithOneArgumentNonConstant) {
  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"((() -> {
      f := (b: bool) -> bool { return b }
      return f(true)
    })())"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"((() -> {
      f := (b: bool) -> bool { return b }
      return f(b = true)
    })())"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"((() -> {
      f := (b: bool) -> bool { return b }
      return true'f
    })())"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"((() -> {
      f := (b: bool) -> bool { return b }
      return (b = true)'f
    })())"));
  }
}

TEST(Call, EvaluationWithImplicitCastNonConstant) {
  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"((() -> {
      f := (n: i64) -> bool { return n < 10 as i64 }
      return f(3)
    })())"));
  }

  {
    test::Repl repl;
    EXPECT_TRUE(repl.execute<bool>(R"((() -> {
      f := (n: i64) -> bool { return n < 10 as i64 }
      return f(n = 3)
    })())"));
  }

  {
    test::Repl repl;
    EXPECT_FALSE(repl.execute<bool>(R"((() -> {
      f := (n: i64) -> bool { return n < 10 as i64 }
      return 10'f
    })())"));
  }

  {
    test::Repl repl;
    EXPECT_FALSE(repl.execute<bool>(R"((() -> {
      f := (n: i64) -> bool { return n < 10 as i64 }
      return (n = 10)'f
    })())"));
  }
}

TEST(Call, MultipleArgumentsNonConstant) {
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return f(3, 4)
    })())"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return f(3, n = 4)
    })())"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return f(m = 3, n = 4)
    })())"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return f(n = 3, m = 4)
    })())"),
              13);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return 3'f(4)
    })())"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return 3'f(n = 4)
    })())"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return (m = 3)'f(n = 4)
    })())"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return (n = 3)'f(m = 4)
    })())"),
              13);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return (3, 4)'f
    })())"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return (3, n = 4)'f
    })())"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return (m = 3, n = 4)'f
    })())"),
              19);
  }
  {
    test::Repl repl;
    EXPECT_EQ(repl.execute<int64_t>(R"((() -> {
      f := (m: i64, n: i64) -> i64 { return m + n * n }
      return (n = 3, m = 4)'f
    })())"),
              13);
  }
}

}  // namespace
}  // namespace semantic_analysis
