#include "gtest/gtest.h"
#include "test/repl.h"
#include "vm/execute.h"

namespace semantic_analysis {
namespace {

using ComparisonOperatorTest = testing::TestWithParam<core::Type>;

template <typename T>
core::Type TypeFor() {
  static constexpr auto type = nth::type<T>;
  if constexpr (type == nth::type<int8_t>) { return I(8); }
  if constexpr (type == nth::type<int16_t>) { return I(16); }
  if constexpr (type == nth::type<int32_t>) { return I(32); }
  if constexpr (type == nth::type<int64_t>) { return I(64); }
  if constexpr (type == nth::type<uint8_t>) { return U(8); }
  if constexpr (type == nth::type<uint16_t>) { return U(16); }
  if constexpr (type == nth::type<uint32_t>) { return U(32); }
  if constexpr (type == nth::type<uint64_t>) { return U(64); }
}

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

using ArithmeticTypes = ::testing::Types<int8_t, int16_t, int32_t, int64_t,
                                         uint8_t, uint16_t, uint32_t, uint64_t>;
template <typename>
using ArithmeticOperatorTest = testing::Test;
TYPED_TEST_SUITE(ArithmeticOperatorTest, ArithmeticTypes);

TYPED_TEST(ArithmeticOperatorTest, Add) {
  core::Type type = TypeFor<TypeParam>();
  test::Repl repl;
  vm::Function const& f = *repl.execute<vm::Function const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> %v { return x + y })",
                      StringOf(type), StringOf(type), StringOf(type)));

  TypeParam result;
  vm::Execute(f, repl.state(), {TypeParam{3}, TypeParam{4}}, result);
  EXPECT_EQ(result, TypeParam{7});

  vm::Execute(f, repl.state(), {TypeParam{0}, TypeParam{3}}, result);
  EXPECT_EQ(result, TypeParam{3});

  vm::Execute(f, repl.state(), {TypeParam{3}, TypeParam{0}}, result);
  EXPECT_EQ(result, TypeParam{3});

  if constexpr (std::is_signed_v<TypeParam>) {
    vm::Execute(f, repl.state(), {TypeParam{3}, TypeParam{-3}}, result);
    EXPECT_EQ(result, TypeParam{0});

    vm::Execute(f, repl.state(),
                {std::numeric_limits<TypeParam>::max(),
                 -std::numeric_limits<TypeParam>::max()},
                result);
    EXPECT_EQ(result, TypeParam{0});
  }
}

TYPED_TEST(ArithmeticOperatorTest, Sub) {
  core::Type type = TypeFor<TypeParam>();
  test::Repl repl;
  vm::Function const& f = *repl.execute<vm::Function const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> %v { return x - y })",
                      StringOf(type), StringOf(type), StringOf(type)));

  TypeParam result;
  vm::Execute(f, repl.state(), {TypeParam{4}, TypeParam{3}}, result);
  EXPECT_EQ(result, TypeParam{1});

  vm::Execute(f, repl.state(), {TypeParam{3}, TypeParam{0}}, result);
  EXPECT_EQ(result, TypeParam{3});

  vm::Execute(f, repl.state(),
              {std::numeric_limits<TypeParam>::max(),
               std::numeric_limits<TypeParam>::max()},
              result);
  EXPECT_EQ(result, TypeParam{0});
}

TYPED_TEST(ArithmeticOperatorTest, Mul) {
  core::Type type = TypeFor<TypeParam>();
  test::Repl repl;
  vm::Function const& f = *repl.execute<vm::Function const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> %v { return x * y })",
                      StringOf(type), StringOf(type), StringOf(type)));

  TypeParam result;
  vm::Execute(f, repl.state(), {TypeParam{4}, TypeParam{3}}, result);
  EXPECT_EQ(result, TypeParam{12});

  vm::Execute(f, repl.state(), {TypeParam{0}, TypeParam{3}}, result);
  EXPECT_EQ(result, TypeParam{0});

  vm::Execute(f, repl.state(), {TypeParam{3}, TypeParam{1}}, result);
  EXPECT_EQ(result, TypeParam{3});

  vm::Execute(f, repl.state(), {TypeParam{1}, TypeParam{3}}, result);
  EXPECT_EQ(result, TypeParam{3});
}

TYPED_TEST(ArithmeticOperatorTest, Div) {
  core::Type type = TypeFor<TypeParam>();
  test::Repl repl;
  vm::Function const& f = *repl.execute<vm::Function const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> %v { return x / y })",
                      StringOf(type), StringOf(type), StringOf(type)));

  TypeParam result;
  vm::Execute(f, repl.state(), {TypeParam{4}, TypeParam{3}}, result);
  EXPECT_EQ(result, TypeParam{1});

  vm::Execute(f, repl.state(), {TypeParam{3}, TypeParam{4}}, result);
  EXPECT_EQ(result, TypeParam{0});

  vm::Execute(f, repl.state(), {TypeParam{6}, TypeParam{3}}, result);
  EXPECT_EQ(result, TypeParam{2});

  vm::Execute(f, repl.state(), {TypeParam{6}, TypeParam{1}}, result);
  EXPECT_EQ(result, TypeParam{6});

  if constexpr (std::is_signed_v<TypeParam>) {
    vm::Execute(f, repl.state(), {TypeParam{6}, TypeParam{-1}}, result);
    EXPECT_EQ(result, TypeParam{-6});
  }
}

TYPED_TEST(ArithmeticOperatorTest, Mod) {
  core::Type type = TypeFor<TypeParam>();
  test::Repl repl;
  vm::Function const& f = *repl.execute<vm::Function const*>(
      absl::StrFormat(R"((x: %v, y: %v) -> %v { return x %% y })",
                      StringOf(type), StringOf(type), StringOf(type)));

  TypeParam result;
  vm::Execute(f, repl.state(), {TypeParam{4}, TypeParam{3}}, result);
  EXPECT_EQ(result, TypeParam{1});

  vm::Execute(f, repl.state(), {TypeParam{3}, TypeParam{4}}, result);
  EXPECT_EQ(result, TypeParam{3});

  vm::Execute(f, repl.state(), {TypeParam{6}, TypeParam{3}}, result);
  EXPECT_EQ(result, TypeParam{0});

  vm::Execute(f, repl.state(), {TypeParam{6}, TypeParam{1}}, result);
  EXPECT_EQ(result, TypeParam{0});
}

TEST(LogicalOperatorTest, All) {
  test::Repl repl;
  EXPECT_TRUE(repl.execute<bool>("true and true"));
  EXPECT_FALSE(repl.execute<bool>("true and false"));
  EXPECT_FALSE(repl.execute<bool>("false and true"));
  EXPECT_FALSE(repl.execute<bool>("false and false"));

  EXPECT_TRUE(repl.execute<bool>("true or true"));
  EXPECT_TRUE(repl.execute<bool>("true or false"));
  EXPECT_TRUE(repl.execute<bool>("false or true"));
  EXPECT_FALSE(repl.execute<bool>("false or false"));

  EXPECT_FALSE(repl.execute<bool>("true xor true"));
  EXPECT_TRUE(repl.execute<bool>("true xor false"));
  EXPECT_TRUE(repl.execute<bool>("false xor true"));
  EXPECT_FALSE(repl.execute<bool>("false xor false"));
}

TEST(LogicalOperatorTest, DISABLED_ShortCircuitingAnd) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  increment_and_return ::= (b: bool, n: *i64) -> bool {
    @n = @n + (1 as i64)
    return b
  }
  (x: bool, y: bool, n: *i64) -> bool { return x and increment_and_return(y, n) }
  )");

  {
    int64_t n = 0;
    bool result;
    vm::Execute(f, repl.state(), {true, true, &n}, result);
    EXPECT_TRUE(result);
    EXPECT_EQ(n, 1);
  }

  {
    int64_t n = 0;
    bool result;
    vm::Execute(f, repl.state(), {true, false, &n}, result);
    EXPECT_FALSE(result);
    EXPECT_EQ(n, 1);
  }

  {
    int64_t n = 0;
    bool result;
    vm::Execute(f, repl.state(), {false, true, &n}, result);
    EXPECT_FALSE(result);
    EXPECT_EQ(n, 1);
  }

  {
    int64_t n = 0;
    bool result;
    vm::Execute(f, repl.state(), {false, false, &n}, result);
    EXPECT_FALSE(result);
    EXPECT_EQ(n, 1);
  }
}

TEST(LogicalOperatorTest, DISABLED_ShortCircuitingOr) {
  test::Repl repl;

  vm::Function const& f = *repl.execute<vm::Function const*>(R"(
  increment_and_return ::= (b: bool, n: *i64) -> bool {
    @n = @n + (1 as i64)
    return b
  }
  (x: bool, y: bool, n: *i64) -> bool { return x or increment_and_return(y, n) }
  )");

  {
    int64_t n = 0;
    bool result;
    vm::Execute(f, repl.state(), {true, true, &n}, result);
    EXPECT_TRUE(result);
    EXPECT_EQ(n, 1);
  }

  {
    int64_t n = 0;
    bool result;
    vm::Execute(f, repl.state(), {true, false, &n}, result);
    EXPECT_FALSE(result);
    EXPECT_EQ(n, 1);
  }

  {
    int64_t n = 0;
    bool result;
    vm::Execute(f, repl.state(), {false, true, &n}, result);
    EXPECT_FALSE(result);
    EXPECT_EQ(n, 1);
  }

  {
    int64_t n = 0;
    bool result;
    vm::Execute(f, repl.state(), {false, false, &n}, result);
    EXPECT_FALSE(result);
    EXPECT_EQ(n, 1);
  }
}

}  // namespace
}  // namespace semantic_analysis
