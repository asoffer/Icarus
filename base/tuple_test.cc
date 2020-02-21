#include "base/tuple.h"

#include "gtest/gtest.h"

namespace base {
namespace {

TEST(Tuple, ForEachSingleArgument) {
  std::tuple nums(1, 2, 3);

  int total = 0;
  tuple::for_each([&total](int arg) { total += arg; }, nums);
  EXPECT_EQ(total, 6);

  tuple::for_each([](auto& arg) { arg = arg * arg; }, nums);
  EXPECT_EQ(nums, std::tuple(1, 4, 9));
}

TEST(Tuple, ForEachMultipleArguments) {
  std::tuple u(1, 2, 3);
  std::tuple v(5, 2, -3);

  int dot_product = 0;
  tuple::for_each([&dot_product](int u, int v) { dot_product += u * v; }, u, v);
  EXPECT_EQ(dot_product, 1 * 5 + 2 * 2 + 3 * -3);
}

TEST(Tuple, TransformSingleArgument) {
  EXPECT_EQ(
      tuple::transform([](auto arg) { return arg * arg; }, std::tuple(1, 2, 3)),
      std::tuple(1, 4, 9));

  EXPECT_EQ(tuple::transform([](auto arg) { return arg + arg; },
                             std::tuple(1, std::string{"hello"}, 3)),
            std::tuple(2, "hellohello", 6));
}

TEST(Tuple, TransformMultipleArguments) {
  EXPECT_EQ(
      tuple::transform([](int num, char c) { return std::string(num, c); },
                       std::tuple(1, 2, 3), std::tuple('x', 'y', 'z')),
      std::tuple("x", "yy", "zzz"));
}

}  // namespace
}  // namespace base
