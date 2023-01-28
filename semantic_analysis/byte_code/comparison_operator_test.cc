#include "gtest/gtest.h"
#include "test/repl.h"

namespace semantic_analysis {
namespace {

using ComparisonOperatorTest = testing::TestWithParam<core::Type>;

std::string_view StringOf(core::Type t) {
  if (t == Bool) { return "bool"; }
  if (t == I(8)) { return "i8"; }
  if (t == I(16)) { return "i16"; }
  if (t == I(32)) { return "i32"; }
  if (t == I(64)) { return "i64"; }
  if (t == U(8)) { return "u8"; }
  if (t == U(16)) { return "u16"; }
  if (t == U(32)) { return "u32"; }
  if (t == U(64)) { return "u64"; }
  if (t == F32) { return "f32"; }
  if (t == F64) { return "f64"; }
  UNREACHABLE();
}

jasmin::Value Value(core::Type t, int n) {
  if (t == I(8)) { return jasmin::Value(static_cast<int8_t>(n)); }
  if (t == I(16)) { return jasmin::Value(static_cast<int16_t>(n)); }
  if (t == I(32)) { return jasmin::Value(static_cast<int32_t>(n)); }
  if (t == I(64)) { return jasmin::Value(static_cast<int64_t>(n)); }
  if (t == U(8)) { return jasmin::Value(static_cast<uint8_t>(n)); }
  if (t == U(16)) { return jasmin::Value(static_cast<uint16_t>(n)); }
  if (t == U(32)) { return jasmin::Value(static_cast<uint32_t>(n)); }
  if (t == U(64)) { return jasmin::Value(static_cast<uint64_t>(n)); }
  if (t == F32) { return jasmin::Value(static_cast<float>(n)); }
  if (t == F64) { return jasmin::Value(static_cast<double>(n)); }
  UNREACHABLE();
}

TEST_P(ComparisonOperatorTest, LessThan) {
  core::Type type = GetParam();

  test::Repl repl;
  IrFunction const& f = *repl.execute<IrFunction const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> bool { return x < y })",
                      StringOf(type), StringOf(type)));

  bool result;
  data_types::IntegerTable table;
  jasmin::ExecutionState<InstructionSet> state{table};

  jasmin::Execute(f, state, {Value(type, 3), Value(type, 4)}, result);
  EXPECT_TRUE(result);

  jasmin::Execute(f, state, {Value(type, 3), Value(type, 3)}, result);
  EXPECT_FALSE(result);

  jasmin::Execute(f, state, {Value(type, 4), Value(type, 3)}, result);
  EXPECT_FALSE(result);
}

TEST_P(ComparisonOperatorTest, LessThanOrEqual) {
  core::Type type = GetParam();

  test::Repl repl;

  IrFunction const& f = *repl.execute<IrFunction const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> bool { return x <= y })",
                      StringOf(type), StringOf(type)));

  bool result;
  data_types::IntegerTable table;
  jasmin::ExecutionState<InstructionSet> state{table};

  jasmin::Execute(f, state, {Value(type, 3), Value(type, 4)}, result);
  EXPECT_TRUE(result);

  jasmin::Execute(f, state, {Value(type, 3), Value(type, 3)}, result);
  EXPECT_TRUE(result);

  jasmin::Execute(f, state, {Value(type, 4), Value(type, 3)}, result);
  EXPECT_FALSE(result);
}

TEST_P(ComparisonOperatorTest, Equal) {
  core::Type type = GetParam();

  test::Repl repl;

  IrFunction const& f = *repl.execute<IrFunction const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> bool { return x == y })",
                      StringOf(type), StringOf(type)));

  bool result;
  data_types::IntegerTable table;
  jasmin::ExecutionState<InstructionSet> state{table};

  jasmin::Execute(f, state, {Value(type, 3), Value(type, 4)}, result);
  EXPECT_FALSE(result);

  jasmin::Execute(f,  state,{Value(type, 3), Value(type, 3)}, result);
  EXPECT_TRUE(result);

  jasmin::Execute(f, state, {Value(type, 4), Value(type, 3)}, result);
  EXPECT_FALSE(result);
}


TEST_P(ComparisonOperatorTest, GreaterThanOrEqual) {
  core::Type type = GetParam();

  test::Repl repl;

  IrFunction const& f = *repl.execute<IrFunction const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> bool { return x >= y })",
                      StringOf(type), StringOf(type)));

  bool result;
  data_types::IntegerTable table;
  jasmin::ExecutionState<InstructionSet> state{table};

  jasmin::Execute(f, state, {Value(type, 3), Value(type, 4)}, result);
  EXPECT_FALSE(result);

  jasmin::Execute(f, state, {Value(type, 3), Value(type, 3)}, result);
  EXPECT_TRUE(result);

  jasmin::Execute(f, state, {Value(type, 4), Value(type, 3)}, result);
  EXPECT_TRUE(result);
}

TEST_P(ComparisonOperatorTest, GreaterThan) {
  core::Type type = GetParam();

  test::Repl repl;

  IrFunction const& f = *repl.execute<IrFunction const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> bool { return x > y })",
                      StringOf(type), StringOf(type)));

  bool result;
  data_types::IntegerTable table;
  jasmin::ExecutionState<InstructionSet> state{table};

  jasmin::Execute(f, state, {Value(type, 3), Value(type, 4)}, result);
  EXPECT_FALSE(result);

  jasmin::Execute(f, state, {Value(type, 3), Value(type, 3)}, result);
  EXPECT_FALSE(result);

  jasmin::Execute(f, state, {Value(type, 4), Value(type, 3)}, result);
  EXPECT_TRUE(result);
}

INSTANTIATE_TEST_SUITE_P(All, ComparisonOperatorTest,
                         testing::ValuesIn({I(8), I(16), I(32), I(64), U(8),
                                            U(16), U(32), U(64), F32, F64}));

TEST(ComparisonOperatorTest, Chains) {
  test::Repl repl;

  IrFunction const& f = *repl.execute<IrFunction const*>(
      R"((x: i64, y: i64, z: i64) -> bool { return x > y == z })");

  bool result;
  data_types::IntegerTable table;
  jasmin::ExecutionState<InstructionSet> state{table};

  jasmin::Execute(f, state, {int64_t{4}, int64_t{4}, int64_t{4}}, result);
  EXPECT_FALSE(result);
  jasmin::Execute(f, state, {int64_t{3}, int64_t{4}, int64_t{4}}, result);
  EXPECT_FALSE(result);

  jasmin::Execute(f, state, {int64_t{4}, int64_t{3}, int64_t{4}}, result);
  EXPECT_FALSE(result);
  jasmin::Execute(f, state, {int64_t{3}, int64_t{3}, int64_t{4}}, result);
  EXPECT_FALSE(result);

  jasmin::Execute(f, state, {int64_t{4}, int64_t{4}, int64_t{3}}, result);
  EXPECT_FALSE(result);
  jasmin::Execute(f, state, {int64_t{3}, int64_t{4}, int64_t{3}}, result);
  EXPECT_FALSE(result);

  jasmin::Execute(f, state, {int64_t{4}, int64_t{3}, int64_t{3}}, result);
  EXPECT_TRUE(result);
  jasmin::Execute(f, state, {int64_t{3}, int64_t{3}, int64_t{3}}, result);
  EXPECT_FALSE(result);
}

TEST(ComparisonOperatorTest, DISABLED_Casting) {
  test::Repl repl;

  IrFunction const& f = *repl.execute<IrFunction const*>(
      R"((x: i64, y: i64, z: i32) -> bool { return x > y })");

  bool result;
  data_types::IntegerTable table;
  jasmin::ExecutionState<InstructionSet> state{table};

  jasmin::Execute(f, state, {int64_t{4}, int32_t{3}}, result);
  EXPECT_TRUE(result);
  jasmin::Execute(f, state, {int64_t{4}, int32_t{4}}, result);
  EXPECT_FALSE(result);
  jasmin::Execute(f, state, {int64_t{4}, int32_t{5}}, result);
  EXPECT_FALSE(result);
}

}  // namespace
}  // namespace semantic_analysis
