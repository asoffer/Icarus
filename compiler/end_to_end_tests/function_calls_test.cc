#include "compiler/compiler.h"
#include "gtest/gtest.h"
#include "ir/interpretter/evaluate.h"
#include "test/module.h"

// TODO handle evaluation failures.
ir::Value EvaluateCall(
    std::string fn, std::vector<std::string> pos_args,
    absl::flat_hash_map<std::string, std::string> named_args) {
  test::TestModule mod;
  auto const *expr = test::MakeCall(std::move(fn), std::move(pos_args),
                                    std::move(named_args), &mod);
  auto value       = mod.compiler.Evaluate(
      type::Typed(expr, mod.data().qual_type(expr)->type()));
  ASSERT(static_cast<bool>(value) == true);
  return *std::move(value);
}

// TEST(FunctionCall, ShortNoArgFunctions) {
//   EXPECT_EQ(EvaluateCall("() => 3", {}, {}), ir::Value(int64_t{3}));
//   EXPECT_EQ(EvaluateCall("() => true", {}, {}), ir::Value(true));
//   EXPECT_EQ(EvaluateCall("() => 3.14", {}, {}), ir::Value(3.14));
//   // TODO multiple returns
// }
//
TEST(FunctionCall, ShortOneArgFunctions) {
  EXPECT_EQ(EvaluateCall("(x: int64) => x * x", {"3"}, {}),
            ir::Value(int64_t{9}));
  EXPECT_EQ(EvaluateCall("(x: int64) => x * x", {}, {{"x", "3"}}),
            ir::Value(int64_t{9}));

  EXPECT_EQ(EvaluateCall("(x: float64) => x * x", {"3.0"}, {}), ir::Value(9.0));
  EXPECT_EQ(EvaluateCall("(x: float64) => x * x", {}, {{"x", "3.0"}}),
            ir::Value(9.0));

  EXPECT_EQ(EvaluateCall("(x: int64 = 2) => x * x", {}, {}),
            ir::Value(int64_t{4}));
  EXPECT_EQ(EvaluateCall("(x: int64 = 2) => x * x", {"3"}, {}),
            ir::Value(int64_t{9}));
  EXPECT_EQ(EvaluateCall("(x: int64 = 2) => x * x", {}, {{"x", "3"}}),
            ir::Value(int64_t{9}));

  EXPECT_EQ(EvaluateCall("(x: float64 = 1.0) => x * x", {}, {}),
            ir::Value(1.0));
  EXPECT_EQ(EvaluateCall("(x: float64 = 1.0) => x * x", {"3.0"}, {}),
            ir::Value(9.0));
  EXPECT_EQ(EvaluateCall("(x: float64 = 1.0) => x * x", {}, {{"x", "3.0"}}),
            ir::Value(9.0));
  // TODO multiple returns
}

TEST(FunctionCall, ShortGenericFunctions) {
  EXPECT_EQ(EvaluateCall("(x: $x) => x * x", {"3"}, {}), ir::Value(int64_t{9}));
  EXPECT_EQ(EvaluateCall("(x: $x) => x * x", {"3.0"}, {}), ir::Value(9.0));
  EXPECT_EQ(EvaluateCall("(x: $x = 1.5) => x * x", {"3"}, {}),
            ir::Value(int64_t{9}));
  EXPECT_EQ(EvaluateCall("(x: $x = 1.5) => x * x", {"3.0"}, {}),
            ir::Value(9.0));
  EXPECT_EQ(EvaluateCall("(x: $x = 1.5) => x * x", {}, {}), ir::Value(2.25));
}

TEST(FunctionCall, ShortMultiArgFunctions) {
  EXPECT_EQ(EvaluateCall("(x: int64, y: int64) => x * y", {"3", "4"}, {}),
            ir::Value(int64_t{12}));
  EXPECT_EQ(EvaluateCall("(x: int64, y: int64) => x * y", {"3"}, {{"y", "4"}}),
            ir::Value(int64_t{12}));
  EXPECT_EQ(EvaluateCall("(x: int64, y: int64) => x * y", {},
                         {{"x", "3"}, {"y", "4"}}),
            ir::Value(int64_t{12}));

  EXPECT_EQ(
      EvaluateCall("(x: float64, y: float64) => x * y", {"3.0", "4.0"}, {}),
      ir::Value(12.0));
  EXPECT_EQ(EvaluateCall("(x: float64, y: float64) => x * y", {"3.0"},
                         {{"y", "4.0"}}),
            ir::Value(12.0));
  EXPECT_EQ(EvaluateCall("(x: float64, y: float64) => x * y", {},
                         {{"x", "3.0"}, {"y", "4.0"}}),
            ir::Value(12.0));
  // TODO multiple returns
}

TEST(FunctionCall, LongNoArgFunctions) {
  EXPECT_EQ(EvaluateCall("() -> int64 { return 3 }", {}, {}),
            ir::Value(int64_t{3}));
  EXPECT_EQ(EvaluateCall("() -> bool { return true }", {}, {}),
            ir::Value(true));
  EXPECT_EQ(EvaluateCall("() -> float64 { return 3.14 }", {}, {}),
            ir::Value(3.14));

  // TODO multiple returns
}

TEST(FunctionCall, LongOneArgFunctions) {
  EXPECT_EQ(EvaluateCall("(x: int64) -> int64 { return x * x }", {"3"}, {}),
            ir::Value(int64_t{9}));
  EXPECT_EQ(
      EvaluateCall("(x: int64) -> int64 { return x * x }", {}, {{"x", "3"}}),
      ir::Value(int64_t{9}));

  EXPECT_EQ(
      EvaluateCall("(x: float64) -> float64 { return x * x }", {"3.0"}, {}),
      ir::Value(9.0));
  EXPECT_EQ(EvaluateCall("(x: float64) -> float64 { return x * x }", {},
                         {{"x", "3.0"}}),
            ir::Value(9.0));

  EXPECT_EQ(EvaluateCall("(x: int64 = 2) -> int64 { return x * x }", {}, {}),
            ir::Value(int64_t{4}));
  EXPECT_EQ(EvaluateCall("(x: int64 = 2) -> int64 { return x * x }", {"3"}, {}),
            ir::Value(int64_t{9}));
  EXPECT_EQ(EvaluateCall("(x: int64 = 2) -> int64 { return x * x }", {},
                         {{"x", "3"}}),
            ir::Value(int64_t{9}));

  EXPECT_EQ(
      EvaluateCall("(x: float64 = 1.0) -> float64 { return x * x }", {}, {}),
      ir::Value(1.0));
  EXPECT_EQ(EvaluateCall("(x: float64 = 1.0) -> float64 { return x * x }",
                         {"3.0"}, {}),
            ir::Value(9.0));
  EXPECT_EQ(EvaluateCall("(x: float64 = 1.0) -> float64 { return x * x }", {},
                         {{"x", "3.0"}}),
            ir::Value(9.0));
  // TODO multiple returns
}

TEST(FunctionCall, LongMultiArgFunctions) {
  EXPECT_EQ(EvaluateCall("(x: int64, y: int64) -> int64 { return x * y }",
                         {"3", "4"}, {}),
            ir::Value(int64_t{12}));
  EXPECT_EQ(EvaluateCall("(x: int64, y: int64) -> int64 { return x * y }",
                         {"3"}, {{"y", "4"}}),
            ir::Value(int64_t{12}));
  EXPECT_EQ(EvaluateCall("(x: int64, y: int64) -> int64 { return x * y }", {},
                         {{"x", "3"}, {"y", "4"}}),
            ir::Value(int64_t{12}));

  EXPECT_EQ(EvaluateCall("(x: float64, y: float64) -> float64 { return x * y }",
                         {"3.0", "4.0"}, {}),
            ir::Value(12.0));
  EXPECT_EQ(EvaluateCall("(x: float64, y: float64) -> float64 { return x * y }",
                         {"3.0"}, {{"y", "4.0"}}),
            ir::Value(12.0));
  EXPECT_EQ(EvaluateCall("(x: float64, y: float64) -> float64 { return x * y }",
                         {}, {{"x", "3.0"}, {"y", "4.0"}}),
            ir::Value(12.0));
  // TODO multiple returns
}

TEST(FunctionCall, LongGenericFunctions) {
  EXPECT_EQ(EvaluateCall("(x: $x) -> $x { return x * x }", {"3"}, {}),
            ir::Value(int64_t{9}));
  EXPECT_EQ(EvaluateCall("(x: $x) -> $x { return x * x }", {"3.0"}, {}),
            ir::Value(9.0));
  EXPECT_EQ(EvaluateCall("(x: $x = 1.5) -> $x { return x * x }", {"3"}, {}),
            ir::Value(int64_t{9}));
  EXPECT_EQ(EvaluateCall("(x: $x = 1.5) -> $x { return x * x }", {"3.0"}, {}),
            ir::Value(9.0));
  EXPECT_EQ(EvaluateCall("(x: $x = 1.5) -> $x { return x * x }", {}, {}),
            ir::Value(2.25));
}
