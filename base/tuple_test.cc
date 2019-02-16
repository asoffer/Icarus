#include "base/tuple.h"

#include "test/test.h"

namespace base {
namespace {
using ::matcher::Eq;
using ::matcher::Tuple;

TEST(TupleForEach) {
  std::tuple nums(1, 2, 3);

  int total = 0;
  tuple::for_each([&total](int arg) { total += arg; }, nums);
  CHECK(total == 6);

  tuple::for_each([](auto& arg) { arg = arg * arg; }, nums);
  CHECK(nums, Tuple(Eq(1), Eq(4), Eq(9)));
}

TEST(TupleForEachTogether) {
  std::tuple u(1, 2, 3);
  std::tuple v(5, 2, -3);

  int dot_product = 0;
  tuple::for_each([&dot_product](int u, int v) { dot_product += u * v; }, u, v);
  CHECK(dot_product == 1 * 5 + 2 * 2 + 3 * -3);
}

TEST(TupleTransform) {
  CHECK(
      tuple::transform([](auto arg) { return arg * arg; }, std::tuple(1, 2, 3)),
      Tuple(Eq(1), Eq(4), Eq(9)));

  CHECK(tuple::transform([](auto arg) { return arg + arg; },
                         std::tuple(1, std::string{"hello"}, 3)),
        Tuple(Eq(2), Eq("hellohello"), Eq(6)));
}

TEST(TupleTransformTogether) {
  CHECK(tuple::transform([](int num, char c) { return std::string(num, c); },
                         std::tuple(1, 2, 3), std::tuple('x', 'y', 'z')),
        Tuple(Eq("x"), Eq("yy"), Eq("zzz")));
}

}  // namespace
}  // namespace base
