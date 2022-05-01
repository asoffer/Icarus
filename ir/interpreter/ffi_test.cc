#include "ir/interpreter/ffi.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "type/function.h"
#include "type/primitive.h"

namespace ir::interpreter {
namespace {

using ::testing::ElementsAre;

TEST(InvokeForeignFunction, Void) {
  static int n = 0;
  auto f       = +[] { ++n; };
  auto* t      = type::Func({}, {});

  InvokeForeignFunction(*t, f, CompleteResultBuffer(), nullptr);
  EXPECT_EQ(n, 1);

  InvokeForeignFunction(*t, f, CompleteResultBuffer(), nullptr);
  EXPECT_EQ(n, 2);
}

TEST(InvokeForeignFunction, ReturnsChar) {
  static int32_t c = 'a';
  auto f           = +[]() -> char { return ++c; };
  auto* t          = type::Func({}, {type::Char});

  char result;
  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer(), &result));
  EXPECT_EQ(c, 'b');
  EXPECT_EQ(result, 'b');

  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer(), &result));
  EXPECT_EQ(c, 'c');
  EXPECT_EQ(result, 'c');
}

TEST(InvokeForeignFunction, ReturnsInt32) {
  static int32_t n = 0;
  auto f           = +[]() -> int32_t { return ++n; };
  auto* t          = type::Func({}, {type::I32});

  int32_t result;
  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer(), &result));
  EXPECT_EQ(n, 1);
  EXPECT_EQ(result, 1);

  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer(), &result));
  EXPECT_EQ(n, 2);
  EXPECT_EQ(result, 2);
}

TEST(InvokeForeignFunction, ReturnsUint64) {
  static uint64_t n = 0;
  auto f            = +[]() -> uint64_t { return ++n; };
  auto* t           = type::Func({}, {type::U64});

  int64_t result;
  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer(), &result));
  EXPECT_EQ(n, 1);
  EXPECT_EQ(result, 1);

  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer(), &result));
  EXPECT_EQ(n, 2);
  EXPECT_EQ(result, 2);
}

TEST(InvokeForeignFunction, AcceptsI32) {
  static int32_t n = 0;
  auto f           = +[](int32_t x) { n = x; };
  auto* t          = type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::I32))}, {});

  ASSERT_TRUE(
      InvokeForeignFunction(*t, f, CompleteResultBuffer(int32_t{17}), nullptr));
  EXPECT_EQ(n, 17);

  ASSERT_TRUE(
      InvokeForeignFunction(*t, f, CompleteResultBuffer(int32_t{34}), nullptr));
  EXPECT_EQ(n, 34);
}

TEST(InvokeForeignFunction, AcceptsI64) {
  static int64_t n = 0;
  auto f           = +[](int64_t x) { n = x; };
  auto* t          = type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::I64))}, {});

  ASSERT_TRUE(
      InvokeForeignFunction(*t, f, CompleteResultBuffer(int64_t{17}), nullptr));
  EXPECT_EQ(n, 17);

  ASSERT_TRUE(
      InvokeForeignFunction(*t, f, CompleteResultBuffer(int64_t{34}), nullptr));
  EXPECT_EQ(n, 34);
}

TEST(InvokeForeignFunction, AcceptsChar) {
  static char c = 0;
  auto f        = +[](char x) { c = x; };
  auto* t       = type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::Char))}, {});

  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer('a'), nullptr));
  EXPECT_EQ(c, 'a');

  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer('z'), nullptr));
  EXPECT_EQ(c, 'z');
}

TEST(InvokeForeignFunction, AcceptsMultipleAndReturns) {
  auto f  = +[](double x, double y, double z) -> double { return x * y + z; };
  auto* t = type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::F64)),
       core::AnonymousParameter(type::QualType::NonConstant(type::F64)),
       core::AnonymousParameter(type::QualType::NonConstant(type::F64))},
      {type::F64});

  double result;
  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer(3.0, 5.0, 7.0),
                                    &result));
  EXPECT_EQ(result, f(3.0, 5.0, 7.0));

  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer(3.5, -7.0, 1.2),
                                    &result));
  EXPECT_EQ(result, f(3.5, -7.0, 1.2));
}

TEST(InvokeForeignFunction, ReturnPromotion) {
  auto f  = +[]() -> char { return 'a'; };
  auto* t = type::Func({}, {type::Char});

  alignas(int32_t) std::array<char, 4> chars = {'w', 'x', 'y', 'z'};
  ASSERT_TRUE(InvokeForeignFunction(*t, f, CompleteResultBuffer(), &chars[0]));
  EXPECT_THAT(chars, ElementsAre('a', 'x', 'y', 'z'));
}

}  // namespace
}  // namespace ir::interpreter
