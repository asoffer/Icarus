#include "frontend/lex/numbers.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace frontend {
namespace {
using ::testing::VariantWith;

TEST(ParseNumber, Base2Integer) {
  EXPECT_THAT(ParseNumber("0b0"), VariantWith<nth::Integer>(nth::Integer(0)));
  EXPECT_THAT(ParseNumber("0b1"), VariantWith<nth::Integer>(nth::Integer(1)));
  EXPECT_THAT(ParseNumber("0b01"), VariantWith<nth::Integer>(nth::Integer(1)));
  EXPECT_THAT(ParseNumber("0b0__1"),
              VariantWith<nth::Integer>(nth::Integer(1)));
  EXPECT_THAT(ParseNumber("0b10"), VariantWith<nth::Integer>(nth::Integer(2)));
  EXPECT_THAT(ParseNumber("0b10_"), VariantWith<nth::Integer>(nth::Integer(2)));
  EXPECT_THAT(ParseNumber("0b__10"),
              VariantWith<nth::Integer>(nth::Integer(2)));
  EXPECT_THAT(ParseNumber("0b010"), VariantWith<nth::Integer>(nth::Integer(2)));
  EXPECT_THAT(ParseNumber("0b01____________________________________0"),
              VariantWith<nth::Integer>(nth::Integer(2)));
  EXPECT_THAT(
      ParseNumber(
          "0b0000000000000000000000000000000000000000000000000000000000000000"),
      VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(
      ParseNumber(
          "0b111111111111111111111111111111111111111111111111111111111111111"),
      VariantWith<nth::Integer>(
          nth::Integer(std::numeric_limits<int64_t>::max())));
  EXPECT_THAT(ParseNumber("0b111_1111_1111_1111_1111_1111_1111_1111_1111_1111_"
                          "1111_1111_1111_1111_1111_1111"),
              VariantWith<nth::Integer>(
                  nth::Integer(std::numeric_limits<int64_t>::max())));
  EXPECT_THAT(
      ParseNumber(
          "0b1000000000000000000000000000000000000000000000000000000000000000"),
      VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("0b"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0b_"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
}

TEST(ParseNumber, Base8Integer) {
  EXPECT_THAT(ParseNumber("0o0"), VariantWith<nth::Integer>(nth::Integer(0)));
  EXPECT_THAT(ParseNumber("0o1"), VariantWith<nth::Integer>(nth::Integer(1)));
  EXPECT_THAT(ParseNumber("0o07"), VariantWith<nth::Integer>(nth::Integer(7)));
  EXPECT_THAT(ParseNumber("0o0_7"), VariantWith<nth::Integer>(nth::Integer(7)));
  EXPECT_THAT(ParseNumber("0o01"), VariantWith<nth::Integer>(nth::Integer(1)));
  EXPECT_THAT(ParseNumber("0o11"), VariantWith<nth::Integer>(nth::Integer(9)));
  EXPECT_THAT(ParseNumber("0o_11"), VariantWith<nth::Integer>(nth::Integer(9)));
  EXPECT_THAT(ParseNumber("0o11__"),
              VariantWith<nth::Integer>(nth::Integer(9)));
  EXPECT_THAT(ParseNumber("0o17777777777"),
              VariantWith<nth::Integer>(
                  nth::Integer(std::numeric_limits<int32_t>::max())));
  EXPECT_THAT(ParseNumber("0o177______________77777_____________777"),
              VariantWith<nth::Integer>(
                  nth::Integer(std::numeric_limits<int32_t>::max())));
  EXPECT_THAT(ParseNumber("0o2000000000000000000000"),
              VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("0o37777777777777777777777777777777"),
              VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("0o77_77_77_77_77_77_77_77_77_77_77_77_77_77_77_77"),
              VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("0o"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0o_"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
}

TEST(ParseNumber, Base10Integer) {
  EXPECT_THAT(ParseNumber("0d0"), VariantWith<nth::Integer>(nth::Integer(0)));
  EXPECT_THAT(ParseNumber("0d07"), VariantWith<nth::Integer>(nth::Integer(7)));
  EXPECT_THAT(ParseNumber("0d01"), VariantWith<nth::Integer>(nth::Integer(1)));
  EXPECT_THAT(ParseNumber("0d11"), VariantWith<nth::Integer>(nth::Integer(11)));
  EXPECT_THAT(ParseNumber("0d9223372036854775807"),
              VariantWith<nth::Integer>(
                  nth::Integer(std::numeric_limits<int64_t>::max())));
  EXPECT_THAT(ParseNumber("0d9223372036854775808"),
              VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("0d9999999999999999999"),
              VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("0"), VariantWith<nth::Integer>(nth::Integer(0)));
  EXPECT_THAT(ParseNumber("7"), VariantWith<nth::Integer>(nth::Integer(7)));
  EXPECT_THAT(ParseNumber("1"), VariantWith<nth::Integer>(nth::Integer(1)));
  EXPECT_THAT(ParseNumber("11"), VariantWith<nth::Integer>(nth::Integer(11)));
  EXPECT_THAT(ParseNumber("9223372036854775807"),
              VariantWith<nth::Integer>(
                  nth::Integer(std::numeric_limits<int64_t>::max())));
  EXPECT_THAT(ParseNumber("9223372036854775808"),
              VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("9999999999999999999"),
              VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("0d"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0d_"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
}

TEST(ParseNumber, Base16Integer) {
  EXPECT_THAT(ParseNumber("0x0"), VariantWith<nth::Integer>(nth::Integer(0)));
  EXPECT_THAT(ParseNumber("0x07"), VariantWith<nth::Integer>(nth::Integer(7)));
  EXPECT_THAT(ParseNumber("0x01"), VariantWith<nth::Integer>(nth::Integer(1)));
  EXPECT_THAT(ParseNumber("0x11"), VariantWith<nth::Integer>(nth::Integer(17)));
  EXPECT_THAT(ParseNumber("0x7fffffff"),
              VariantWith<nth::Integer>(
                  nth::Integer(std::numeric_limits<int32_t>::max())));
  EXPECT_THAT(ParseNumber("0x80000000"),
              VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("0xffffffff"),
              VariantWith<NumberParsingError>(NumberParsingError::kTooLarge));
  EXPECT_THAT(ParseNumber("0x"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0x_"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
}

TEST(ParseNumber, Base2Real) {
  EXPECT_THAT(ParseNumber("0b0.0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber("0b.0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber("0b1."), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber("0b1.0"), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber("0b.1"), VariantWith<double>(0.5));
  EXPECT_THAT(ParseNumber("0b.001"), VariantWith<double>(0.125));
  EXPECT_THAT(ParseNumber("0b_1__0_.1_"), VariantWith<double>(2.5));
  EXPECT_THAT(ParseNumber("0b_._"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0b."),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0b.10.10"), VariantWith<NumberParsingError>(
                                           NumberParsingError::kTooManyDots));
  // TODO overflow and underflow
}

TEST(ParseNumber, Base8Real) {
  EXPECT_THAT(ParseNumber("0o0.0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber("0o.0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber("0o1."), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber("0o1.0"), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber("0o.1"), VariantWith<double>(0.125));
  EXPECT_THAT(ParseNumber("0o.001"), VariantWith<double>(0.001953125));
  EXPECT_THAT(ParseNumber("0o.04"), VariantWith<double>(0.0625));
  EXPECT_THAT(ParseNumber("0o_1__1_.2_"), VariantWith<double>(9.25));
  EXPECT_THAT(ParseNumber("0o_._"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0o."),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0o.12.34"), VariantWith<NumberParsingError>(
                                           NumberParsingError::kTooManyDots));
  // TODO overflow and underflow
}

TEST(ParseNumber, Base10Real) {
  EXPECT_THAT(ParseNumber("0.0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber(".0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber("1."), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber("1.0"), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber(".1"), VariantWith<double>(0.1));
  EXPECT_THAT(ParseNumber(".001"), VariantWith<double>(0.001));
  EXPECT_THAT(ParseNumber(".04"), VariantWith<double>(0.04));
  EXPECT_THAT(ParseNumber("_1__1_.2_"), VariantWith<double>(11.2));

  EXPECT_THAT(ParseNumber("0d0.0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber("0d.0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber("0d1."), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber("0d1.0"), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber("0d.1"), VariantWith<double>(0.1));
  EXPECT_THAT(ParseNumber("0d.001"), VariantWith<double>(0.001));
  EXPECT_THAT(ParseNumber("0d.04"), VariantWith<double>(0.04));
  EXPECT_THAT(ParseNumber("0d_1__1_.2_"), VariantWith<double>(11.2));
  EXPECT_THAT(ParseNumber("0d_._"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0d."),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0.12.34"), VariantWith<NumberParsingError>(
                                          NumberParsingError::kTooManyDots));
  EXPECT_THAT(ParseNumber("0d.12.34"), VariantWith<NumberParsingError>(
                                           NumberParsingError::kTooManyDots));
  // TODO overflow and underflow
}

TEST(ParseNumber, Base16Real) {
  EXPECT_THAT(ParseNumber("0x0.0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber("0x.0"), VariantWith<double>(0.0));
  EXPECT_THAT(ParseNumber("0x1."), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber("0x1.0"), VariantWith<double>(1.0));
  EXPECT_THAT(ParseNumber("0x.1"), VariantWith<double>(0.0625));
  EXPECT_THAT(ParseNumber("0x.04"), VariantWith<double>(0.015625));
  EXPECT_THAT(ParseNumber("0x_1__1_.2_"), VariantWith<double>(17.125));
  EXPECT_THAT(ParseNumber("0x_._"),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0x."),
              VariantWith<NumberParsingError>(NumberParsingError::kNoDigits));
  EXPECT_THAT(ParseNumber("0x.12.34"), VariantWith<NumberParsingError>(
                                           NumberParsingError::kTooManyDots));
  // TODO overflow and underflow
}

}  // namespace
}  // namespace frontend
