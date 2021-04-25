#include "absl/strings/str_format.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "test/module.h"
#include "type/primitive.h"

namespace compiler {
namespace {

struct TestCase {
  std::string op;
  std::string type;
};

struct TestData {
  std::string lhs;
  std::string rhs;
  std::string type;
  ir::Value expected;
};

constexpr char const *kCommonDefinitions = R"(
  Color ::= flags {
    RED   ::= 1 as u64
    GREEN ::= 2 as u64
    BLUE  ::= 4 as u64
  }
)";

using BinaryOperatorTest =
    testing::TestWithParam<std::tuple<TestCase, TestData>>;
TEST_P(BinaryOperatorTest, Constants) {
  auto const &[test_case, test_data] = GetParam();
  test::TestModule mod;
  // TODO: We can't use `s` as the field member because the compiler thinks
  // there's an ambiguity (there isn't).
  mod.AppendCode(kCommonDefinitions);
  auto const *e  = mod.Append<ast::Expression>(absl::StrFormat(
      R"((%s) %s (%s))", test_data.lhs, test_case.op, test_data.rhs));
  auto t         = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, test_data.expected);
}

TEST_P(BinaryOperatorTest, NonConstants) {
  auto const &[test_case, test_data] = GetParam();
  test::TestModule mod;
  // TODO: We can't use `s` as the field member because the compiler thinks
  // there's an ambiguity (there isn't).
  mod.AppendCode(kCommonDefinitions);
  auto const *e  = mod.Append<ast::Expression>(absl::StrFormat(
      R"(
      (() -> %s {
        lhs := %s
        rhs := %s
        return lhs %s rhs
      })()
      )",
      test_case.type, test_data.lhs, test_data.rhs, test_case.op));
  auto t = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, test_data.expected);
}

TEST_P(BinaryOperatorTest, Assignment) {
  auto const &[test_case, test_data] = GetParam();
  // TODO: Find a better way to do this.
  if (std::isalpha(test_case.op[0])) GTEST_SKIP();

  test::TestModule mod;
  // TODO: We can't use `s` as the field member because the compiler thinks
  // there's an ambiguity (there isn't).
  mod.AppendCode(kCommonDefinitions);
  std::string s;
  auto const *e  = mod.Append<ast::Expression>(absl::StrFormat(
      R"(
      (() -> %s {
        lhs := %s
        lhs %s= %s
        return lhs
      })()
      )",
      test_case.type, test_data.lhs, test_case.op, test_data.rhs));
  auto t = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, test_data.expected);
}

// Note: We test both with literals and with a unary-operator applied directly
// to a function call. The former helps cover the constant-folding mechanisms
// built in to the ir::Builder. The latter helps cover the common case for code
// emission.
INSTANTIATE_TEST_SUITE_P(
    BooleanOr, BinaryOperatorTest,
    testing::Combine(
        testing::ValuesIn({TestCase{.op = "or", .type = "bool"}}),
        testing::ValuesIn(std::vector<TestData>{
            {.lhs = "false", .rhs = "false", .expected = ir::Value(false)},
            {.lhs = "true", .rhs = "false", .expected = ir::Value(true)},
            {.lhs = "false", .rhs = "true", .expected = ir::Value(true)},
            {.lhs = "true", .rhs = "true", .expected = ir::Value(true)},
        })));

INSTANTIATE_TEST_SUITE_P(
    BooleanAnd, BinaryOperatorTest,
    testing::Combine(
        testing::ValuesIn({TestCase{.op = "and", .type = "bool"}}),
        testing::ValuesIn(std::vector<TestData>{
            {.lhs = "false", .rhs = "false", .expected = ir::Value(false)},
            {.lhs = "true", .rhs = "false", .expected = ir::Value(false)},
            {.lhs = "false", .rhs = "true", .expected = ir::Value(false)},
            {.lhs = "true", .rhs = "true", .expected = ir::Value(true)},
        })));

INSTANTIATE_TEST_SUITE_P(
    BooleanXor, BinaryOperatorTest,
    testing::Combine(
        testing::ValuesIn({TestCase{.op = "xor", .type = "bool"}}),
        testing::ValuesIn(std::vector<TestData>{
            {.lhs = "false", .rhs = "false", .expected = ir::Value(false)},
            {.lhs = "true", .rhs = "false", .expected = ir::Value(true)},
            {.lhs = "false", .rhs = "true", .expected = ir::Value(true)},
            {.lhs = "true", .rhs = "true", .expected = ir::Value(false)},
        })));

// TODO: Short-circuiting test for boolean `|` and `&`.

INSTANTIATE_TEST_SUITE_P(
    FlagsOr, BinaryOperatorTest,
    testing::Combine(
        testing::ValuesIn({TestCase{.op = "|", .type = "Color"}}),
        testing::ValuesIn({TestData{
            .lhs      = "Color.RED",
            .rhs      = "Color.BLUE",
            .expected = ir::Value(type::Flags::underlying_type(5))}})));

INSTANTIATE_TEST_SUITE_P(
    FlagsAnd, BinaryOperatorTest,
    testing::Combine(
        testing::ValuesIn({TestCase{.op = "&", .type = "Color"}}),
        testing::ValuesIn({TestData{
            .lhs      = "Color.RED | Color.GREEN",
            .rhs      = "Color.BLUE | Color.GREEN",
            .expected = ir::Value(type::Flags::underlying_type(2))}})));

INSTANTIATE_TEST_SUITE_P(
    FlagsXor, BinaryOperatorTest,
    testing::Combine(
        testing::ValuesIn({TestCase{.op = "^", .type = "Color"}}),
        testing::ValuesIn({TestData{
            .lhs      = "Color.RED | Color.GREEN",
            .rhs      = "Color.BLUE | Color.GREEN",
            .expected = ir::Value(type::Flags::underlying_type(5))}})));

template <typename T>
std::string_view TypeName() {
  constexpr auto type = base::meta<T>;
  if constexpr (type == base::meta<bool>) { return "bool"; }
  if constexpr (type == base::meta<int8_t>) { return "i8"; }
  if constexpr (type == base::meta<int16_t>) { return "i16"; }
  if constexpr (type == base::meta<int32_t>) { return "i32"; }
  if constexpr (type == base::meta<int64_t>) { return "i64"; }
  if constexpr (type == base::meta<uint8_t>) { return "u8"; }
  if constexpr (type == base::meta<uint16_t>) { return "u16"; }
  if constexpr (type == base::meta<uint32_t>) { return "u32"; }
  if constexpr (type == base::meta<uint64_t>) { return "u64"; }
  if constexpr (type == base::meta<float>) { return "f32"; }
  if constexpr (type == base::meta<double>) { return "f64"; }
}

template <char Op, typename T>
TestData MakeTestData(T lhs, T rhs) {
  if constexpr (Op == '+') {
    return {
        .lhs      = absl::StrCat(lhs, " as ", TypeName<T>()),
        .rhs      = absl::StrCat(rhs, " as ", TypeName<T>()),
        .expected = ir::Value(static_cast<T>(lhs + rhs)),
    };
  } else if constexpr (Op == '-') {
    return {
        .lhs      = absl::StrCat(lhs, " as ", TypeName<T>()),
        .rhs      = absl::StrCat(rhs, " as ", TypeName<T>()),
        .expected = ir::Value(static_cast<T>(lhs - rhs)),
    };
  } else if constexpr (Op == '*') {
    return {
        .lhs      = absl::StrCat(lhs, " as ", TypeName<T>()),
        .rhs      = absl::StrCat(rhs, " as ", TypeName<T>()),
        .expected = ir::Value(static_cast<T>(lhs * rhs)),
    };
  } else if constexpr (Op == '/') {
    return {
        .lhs      = absl::StrCat(lhs, " as ", TypeName<T>()),
        .rhs      = absl::StrCat(rhs, " as ", TypeName<T>()),
        .expected = ir::Value(static_cast<T>(lhs / rhs)),
    };
  } else if constexpr (Op == '%') {
    return {
        .lhs      = absl::StrCat(lhs, " as ", TypeName<T>()),
        .rhs      = absl::StrCat(rhs, " as ", TypeName<T>()),
        .expected = ir::Value(static_cast<T>(lhs % rhs)),
    };
  }
}

template <typename T>
std::vector<TestData> MakeAddTestDataSet() {
  if constexpr (std::is_signed_v<T>) {
    return {
        MakeTestData<'+'>(T{0}, T{0}), MakeTestData<'+'>(T{0}, T{1}),
        MakeTestData<'+'>(T{1}, T{0}), MakeTestData<'+'>(T{0}, T{-1}),
        MakeTestData<'+'>(T{-1}, T{0}),
        // TODO: This one fails due to a parsing bug with 64-bit integers. In
        // general the negative max value cannot be treated as unary negation of
        // a positive value because that value doesn't have a repreesntation in
        // the same type.
        //
        // MakeTestData<'+'>(std::numeric_limits<T>::min(), T{0}),
        // MakeTestData<'+'>(T{0}, std::numeric_limits<T>::min()),
        MakeTestData<'+'>(std::numeric_limits<T>::max(), T{0}),
        MakeTestData<'+'>(T{0}, std::numeric_limits<T>::max()),
        // MakeTestData<'+'>(std::numeric_limits<T>::max(),
        //                   std::numeric_limits<T>::min()),
        // MakeTestData<'+'>(std::numeric_limits<T>::min(),
        //                   std::numeric_limits<T>::max()),
    };
  } else {
    return {
        MakeTestData<'+'>(T{0}, T{0}),
        MakeTestData<'+'>(T{0}, T{1}),
        MakeTestData<'+'>(T{1}, T{0}),
        // TODO: Similar parsing bug as above.
        // MakeTestData<'+'>(std::numeric_limits<T>::max(), T{0}),
        // MakeTestData<'+'>(T{0}, std::numeric_limits<T>::max()),
        MakeTestData<'+', T>(std::numeric_limits<T>::max() / T{2},
                             std::numeric_limits<T>::min() / T{2}),
        MakeTestData<'+', T>(std::numeric_limits<T>::min() / T{2},
                             std::numeric_limits<T>::max() / T{2}),
    };
  }
}

INSTANTIATE_TEST_SUITE_P(
    I8Add, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "+", .type = "i8"}}),
                     testing::ValuesIn(MakeAddTestDataSet<int8_t>())));
INSTANTIATE_TEST_SUITE_P(
    I16Add, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "+", .type = "i16"}}),
                     testing::ValuesIn(MakeAddTestDataSet<int16_t>())));
INSTANTIATE_TEST_SUITE_P(
    I32Add, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "+", .type = "i32"}}),
                     testing::ValuesIn(MakeAddTestDataSet<int32_t>())));
INSTANTIATE_TEST_SUITE_P(
    I64Add, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "+", .type = "i64"}}),
                     testing::ValuesIn(MakeAddTestDataSet<int64_t>())));

INSTANTIATE_TEST_SUITE_P(
    U8Add, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "+", .type = "u8"}}),
                     testing::ValuesIn(MakeAddTestDataSet<uint8_t>())));
INSTANTIATE_TEST_SUITE_P(
    U16Add, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "+", .type = "u16"}}),
                     testing::ValuesIn(MakeAddTestDataSet<uint16_t>())));
INSTANTIATE_TEST_SUITE_P(
    U32Add, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "+", .type = "u32"}}),
                     testing::ValuesIn(MakeAddTestDataSet<uint32_t>())));
INSTANTIATE_TEST_SUITE_P(
    U64Add, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "+", .type = "u64"}}),
                     testing::ValuesIn(MakeAddTestDataSet<uint64_t>())));

template <typename T>
std::vector<TestData> MakeSubTestDataSet() {
  if constexpr (std::is_signed_v<T>) {
    return {
        MakeTestData<'-'>(T{0}, T{0}),  MakeTestData<'-'>(T{0}, T{1}),
        MakeTestData<'-'>(T{1}, T{0}),  MakeTestData<'-'>(T{0}, T{-1}),
        MakeTestData<'-'>(T{-1}, T{0}),
        // TODO: Similar parsing bug as above.
        // MakeTestData<'-'>(std::numeric_limits<T>::min(), T{0}),
        // MakeTestData<'-'>(std::numeric_limits<T>::max(), T{0}),
        // MakeTestData<'-'>(T{0}, std::numeric_limits<T>::max()),
        // MakeTestData<'-'>(std::numeric_limits<T>::max(),
        //                   std::numeric_limits<T>::max()),
        // MakeTestData<'-'>(std::numeric_limits<T>::min(),
        //                   std::numeric_limits<T>::min()),
    };
  } else {
    return {
        MakeTestData<'-'>(T{0}, T{0}), MakeTestData<'-'>(T{1}, T{0}),
        // TODO: Similar parsing bug as above.
        // MakeTestData<'-'>(std::numeric_limits<T>::max(), T{0}),
        // MakeTestData<'-'>(std::numeric_limits<T>::max(), T{1}),
    };
  }
}

INSTANTIATE_TEST_SUITE_P(
    I8Sub, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "-", .type = "i8"}}),
                     testing::ValuesIn(MakeSubTestDataSet<int8_t>())));
INSTANTIATE_TEST_SUITE_P(
    I16Sub, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "-", .type = "i16"}}),
                     testing::ValuesIn(MakeSubTestDataSet<int16_t>())));
INSTANTIATE_TEST_SUITE_P(
    I32Sub, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "-", .type = "i32"}}),
                     testing::ValuesIn(MakeSubTestDataSet<int32_t>())));
INSTANTIATE_TEST_SUITE_P(
    I64Sub, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "-", .type = "i64"}}),
                     testing::ValuesIn(MakeSubTestDataSet<int64_t>())));

INSTANTIATE_TEST_SUITE_P(
    U8Sub, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "-", .type = "u8"}}),
                     testing::ValuesIn(MakeSubTestDataSet<uint8_t>())));
INSTANTIATE_TEST_SUITE_P(
    U16Sub, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "-", .type = "u16"}}),
                     testing::ValuesIn(MakeSubTestDataSet<uint16_t>())));
INSTANTIATE_TEST_SUITE_P(
    U32Sub, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "-", .type = "u32"}}),
                     testing::ValuesIn(MakeSubTestDataSet<uint32_t>())));
INSTANTIATE_TEST_SUITE_P(
    U64Sub, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "-", .type = "u64"}}),
                     testing::ValuesIn(MakeSubTestDataSet<uint64_t>())));

template <typename T>
std::vector<TestData> MakeMulTestDataSet() {
  if constexpr (std::is_signed_v<T>) {
    return {
        MakeTestData<'*'>(T{0}, T{0}),
        MakeTestData<'*'>(T{0}, T{1}),
        MakeTestData<'*'>(T{1}, T{0}),
        MakeTestData<'*'>(T{0}, T{-1}),
        MakeTestData<'*'>(T{-1}, T{0}),
        MakeTestData<'*'>(T{1}, T{1}),
        MakeTestData<'*'>(T{-1}, T{1}),
        MakeTestData<'*'>(T{1}, T{-1}),
        MakeTestData<'*'>(T{-1}, T{-1}),
        MakeTestData<'*'>(T{0}, std::numeric_limits<T>::max()),
        MakeTestData<'*'>(std::numeric_limits<T>::max(), T{0}),
        MakeTestData<'*'>(T{1}, std::numeric_limits<T>::max()),
        MakeTestData<'*'>(std::numeric_limits<T>::max(), T{1}),
        MakeTestData<'*'>(T{-1}, std::numeric_limits<T>::max()),
        MakeTestData<'*'>(std::numeric_limits<T>::max(), T{-1}),
        MakeTestData<'*'>(T{10}, T{10}),
        MakeTestData<'*'>(T{10}, T{-10}),
        MakeTestData<'*'>(T{-10}, T{10}),
        MakeTestData<'*'>(T{-10}, T{-10}),
        // TODO: Similar parsing bug as above.
        // MakeTestData<'*'>(std::numeric_limits<T>::min(), T{0}),
        // MakeTestData<'*'>(std::numeric_limits<T>::max(), T{0}),
        // MakeTestData<'*'>(T{0}, std::numeric_limits<T>::max()),
        // MakeTestData<'*'>(std::numeric_limits<T>::max(),
        //                   std::numeric_limits<T>::max()),
        // MakeTestData<'*'>(std::numeric_limits<T>::min(),
        //                   std::numeric_limits<T>::min()),
    };
  } else {
    return {
        MakeTestData<'*'>(T{0}, T{0}), MakeTestData<'*'>(T{1}, T{0}),
        MakeTestData<'*'>(T{0}, T{1}), MakeTestData<'*'>(T{1}, T{1}),
        // TODO: Similar parsing bug as above.
        // MakeTestData<'*'>(T{0}, std::numeric_limits<T>::max()),
        // MakeTestData<'*'>(std::numeric_limits<T>::max()}, T{0}),
        // MakeTestData<'*'>(T{1}, std::numeric_limits<T>::max()),
        // MakeTestData<'*'>(std::numeric_limits<T>::max(), T{1}),

        // TODO: Similar parsing bug as above.
        // MakeTestData<'*'>(std::numeric_limits<T>::max(), T{0}),
        // MakeTestData<'*'>(std::numeric_limits<T>::max(), T{1}),
    };
  }
}

INSTANTIATE_TEST_SUITE_P(
    I8Mul, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "*", .type = "i8"}}),
                     testing::ValuesIn(MakeMulTestDataSet<int8_t>())));
INSTANTIATE_TEST_SUITE_P(
    I16Mul, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "*", .type = "i16"}}),
                     testing::ValuesIn(MakeMulTestDataSet<int16_t>())));
INSTANTIATE_TEST_SUITE_P(
    I32Mul, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "*", .type = "i32"}}),
                     testing::ValuesIn(MakeMulTestDataSet<int32_t>())));
INSTANTIATE_TEST_SUITE_P(
    I64Mul, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "*", .type = "i64"}}),
                     testing::ValuesIn(MakeMulTestDataSet<int64_t>())));

INSTANTIATE_TEST_SUITE_P(
    U8Mul, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "*", .type = "u8"}}),
                     testing::ValuesIn(MakeMulTestDataSet<uint8_t>())));
INSTANTIATE_TEST_SUITE_P(
    U16Mul, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "*", .type = "u16"}}),
                     testing::ValuesIn(MakeMulTestDataSet<uint16_t>())));
INSTANTIATE_TEST_SUITE_P(
    U32Mul, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "*", .type = "u32"}}),
                     testing::ValuesIn(MakeMulTestDataSet<uint32_t>())));
INSTANTIATE_TEST_SUITE_P(
    U64Mul, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "*", .type = "u64"}}),
                     testing::ValuesIn(MakeMulTestDataSet<uint64_t>())));

template <typename T>
std::vector<TestData> MakeDivTestDataSet() {
  if constexpr (std::is_signed_v<T>) {
    return {
        MakeTestData<'/'>(T{0}, T{1}),
        MakeTestData<'/'>(T{0}, T{-1}),
        MakeTestData<'/'>(T{1}, T{1}),
        MakeTestData<'/'>(T{-1}, T{1}),
        MakeTestData<'/'>(T{1}, T{-1}),
        MakeTestData<'/'>(T{-1}, T{-1}),
        MakeTestData<'/'>(T{0}, std::numeric_limits<T>::max()),
        MakeTestData<'/'>(T{1}, std::numeric_limits<T>::max()),
        MakeTestData<'/'>(T{-1}, std::numeric_limits<T>::max()),
        MakeTestData<'/'>(std::numeric_limits<T>::max(), T{1}),
        MakeTestData<'/'>(std::numeric_limits<T>::max(), T{-1}),
        MakeTestData<'/'>(std::numeric_limits<T>::max(),
                          std::numeric_limits<T>::max()),
        MakeTestData<'/'>(T{10}, T{11}),
        MakeTestData<'/'>(T{10}, T{-11}),
        MakeTestData<'/'>(T{-10}, T{11}),
        MakeTestData<'/'>(T{-10}, T{-11}),
        MakeTestData<'/'>(T{100}, T{11}),
        MakeTestData<'/'>(T{100}, T{-11}),
        MakeTestData<'/'>(T{-100}, T{11}),
        MakeTestData<'/'>(T{-100}, T{-11}),

        // TODO: Similar parsing bug as above.
        // MakeTestData<'/'>(T{0}, std::numeric_limits<T>::min()),
        // MakeTestData<'/'>(T{1}, std::numeric_limits<T>::min()),
        // MakeTestData<'/'>(T{-1}, std::numeric_limits<T>::min()),
        // MakeTestData<'/'>(std::numeric_limits<T>::min(), T{1}),
        // MakeTestData<'/'>(std::numeric_limits<T>::min(), T{-1}),
        // MakeTestData<'/'>(std::numeric_limits<T>::min(),
        //                   std::numeric_limits<T>::min()),
    };
  } else {
    return {
        MakeTestData<'/'>(T{0}, T{1}), MakeTestData<'/'>(T{1}, T{1}),
        MakeTestData<'/'>(T{5}, T{3}), MakeTestData<'/'>(T{3}, T{5}),
        // TODO: Similar parsing bug as above.
        // MakeTestData<'/'>(T{0}, std::numeric_limits<T>::max()),
        // MakeTestData<'/'>(T{1}, std::numeric_limits<T>::max()),
        // MakeTestData<'/'>(std::numeric_limits<T>::max(), T{1}),
    };
  }
}

INSTANTIATE_TEST_SUITE_P(
    I8Div, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "/", .type = "i8"}}),
                     testing::ValuesIn(MakeDivTestDataSet<int8_t>())));
INSTANTIATE_TEST_SUITE_P(
    I16Div, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "/", .type = "i16"}}),
                     testing::ValuesIn(MakeDivTestDataSet<int16_t>())));
INSTANTIATE_TEST_SUITE_P(
    I32Div, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "/", .type = "i32"}}),
                     testing::ValuesIn(MakeDivTestDataSet<int32_t>())));
INSTANTIATE_TEST_SUITE_P(
    I64Div, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "/", .type = "i64"}}),
                     testing::ValuesIn(MakeDivTestDataSet<int64_t>())));

INSTANTIATE_TEST_SUITE_P(
    U8Div, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "/", .type = "u8"}}),
                     testing::ValuesIn(MakeDivTestDataSet<uint8_t>())));
INSTANTIATE_TEST_SUITE_P(
    U16Div, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "/", .type = "u16"}}),
                     testing::ValuesIn(MakeDivTestDataSet<uint16_t>())));
INSTANTIATE_TEST_SUITE_P(
    U32Div, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "/", .type = "u32"}}),
                     testing::ValuesIn(MakeDivTestDataSet<uint32_t>())));
INSTANTIATE_TEST_SUITE_P(
    U64Div, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "/", .type = "u64"}}),
                     testing::ValuesIn(MakeDivTestDataSet<uint64_t>())));

template <typename T>
std::vector<TestData> MakeModTestDataSet() {
  if constexpr (std::is_signed_v<T>) {
    return {
        MakeTestData<'%'>(T{0}, T{1}),
        MakeTestData<'%'>(T{0}, T{-1}),
        MakeTestData<'%'>(T{1}, T{1}),
        MakeTestData<'%'>(T{-1}, T{1}),
        MakeTestData<'%'>(T{1}, T{-1}),
        MakeTestData<'%'>(T{-1}, T{-1}),
        MakeTestData<'%'>(T{0}, std::numeric_limits<T>::max()),
        MakeTestData<'%'>(T{1}, std::numeric_limits<T>::max()),
        MakeTestData<'%'>(T{-1}, std::numeric_limits<T>::max()),
        MakeTestData<'%'>(std::numeric_limits<T>::max(), T{1}),
        MakeTestData<'%'>(std::numeric_limits<T>::max(), T{-1}),
        MakeTestData<'%'>(std::numeric_limits<T>::max(),
                          std::numeric_limits<T>::max()),
        MakeTestData<'%'>(T{10}, T{11}),
        MakeTestData<'%'>(T{10}, T{-11}),
        MakeTestData<'%'>(T{-10}, T{11}),
        MakeTestData<'%'>(T{-10}, T{-11}),
        MakeTestData<'%'>(T{100}, T{11}),
        MakeTestData<'%'>(T{100}, T{-11}),
        MakeTestData<'%'>(T{-100}, T{11}),
        MakeTestData<'%'>(T{-100}, T{-11}),

        // TODO: Similar parsing bug as above.
        // MakeTestData<'/'>(T{0}, std::numeric_limits<T>::min()),
        // MakeTestData<'/'>(T{1}, std::numeric_limits<T>::min()),
        // MakeTestData<'/'>(T{-1}, std::numeric_limits<T>::min()),
        // MakeTestData<'/'>(std::numeric_limits<T>::min(), T{1}),
        // MakeTestData<'/'>(std::numeric_limits<T>::min(), T{-1}),
        // MakeTestData<'/'>(std::numeric_limits<T>::min(),
        //                   std::numeric_limits<T>::min()),
    };
  } else {
    return {
        MakeTestData<'%'>(T{0}, T{1}), MakeTestData<'%'>(T{1}, T{1}),
        MakeTestData<'%'>(T{5}, T{3}), MakeTestData<'%'>(T{3}, T{5}),
        // TODO: Similar parsing bug as above.
        // MakeTestData<'/'>(T{0}, std::numeric_limits<T>::max()),
        // MakeTestData<'/'>(T{1}, std::numeric_limits<T>::max()),
        // MakeTestData<'/'>(std::numeric_limits<T>::max(), T{1}),
    };
  }
}

INSTANTIATE_TEST_SUITE_P(
    I8Mod, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "%", .type = "i8"}}),
                     testing::ValuesIn(MakeModTestDataSet<int8_t>())));
INSTANTIATE_TEST_SUITE_P(
    I16Mod, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "%", .type = "i16"}}),
                     testing::ValuesIn(MakeModTestDataSet<int16_t>())));
INSTANTIATE_TEST_SUITE_P(
    I32Mod, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "%", .type = "i32"}}),
                     testing::ValuesIn(MakeModTestDataSet<int32_t>())));
INSTANTIATE_TEST_SUITE_P(
    I64Mod, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "%", .type = "i64"}}),
                     testing::ValuesIn(MakeModTestDataSet<int64_t>())));

INSTANTIATE_TEST_SUITE_P(
    U8Mod, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "%", .type = "u8"}}),
                     testing::ValuesIn(MakeModTestDataSet<uint8_t>())));
INSTANTIATE_TEST_SUITE_P(
    U16Mod, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "%", .type = "u16"}}),
                     testing::ValuesIn(MakeModTestDataSet<uint16_t>())));
INSTANTIATE_TEST_SUITE_P(
    U32Mod, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "%", .type = "u32"}}),
                     testing::ValuesIn(MakeModTestDataSet<uint32_t>())));
INSTANTIATE_TEST_SUITE_P(
    U64Mod, BinaryOperatorTest,
    testing::Combine(testing::ValuesIn({TestCase{.op = "%", .type = "u64"}}),
                     testing::ValuesIn(MakeModTestDataSet<uint64_t>())));

struct BufferPointerTestData {
  std::string expr;
  int64_t expected_result;
};
using BufferPointerTest = testing::TestWithParam<BufferPointerTestData>;
TEST_P(BufferPointerTest, Arithmetic) {
  auto const &[expr, expected_result] = GetParam();
  test::TestModule mod;
  // TODO: We can't use `s` as the field member because the compiler thinks
  // there's an ambiguity (there isn't).
  auto const *e  = mod.Append<ast::Expression>(absl::StrFormat(
      R"((() -> i64 {
        a := [1, 2, 3]
        p: [*]i64 = &a[1]
        return %s
      })()
      )", expr));
  auto t         = mod.context().qual_types(e)[0].type();
  ASSERT_TRUE(t.valid());
  auto result =
      mod.compiler.Evaluate(type::Typed<ast::Expression const *>(e, t));
  ASSERT_TRUE(result);
  EXPECT_EQ(*result, ir::Value(expected_result));
}
INSTANTIATE_TEST_SUITE_P(
    Arithmetic, BufferPointerTest,
    testing::ValuesIn({
        BufferPointerTestData{.expr = "@(p - 1)", .expected_result = 1},
        BufferPointerTestData{.expr = "@(p + 0)", .expected_result = 2},
        BufferPointerTestData{.expr = "@(p + 1)", .expected_result = 3},
        BufferPointerTestData{.expr = "@(p - 1 as u8)", .expected_result = 1},
        BufferPointerTestData{.expr = "@(p + 0 as u8)", .expected_result = 2},
        BufferPointerTestData{.expr = "@(p + 1 as u8)", .expected_result = 3},
    }));

// TODO: Floating point tests.

}  // namespace
}  // namespace compiler
