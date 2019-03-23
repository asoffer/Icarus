#include "base/tuple.h"

#include "test/catch.h"

namespace base {
namespace {
TEST_CASE("for_each single argument") {
  std::tuple nums(1, 2, 3);

  int total = 0;
  tuple::for_each([&total](int arg) { total += arg; }, nums);
  CHECK(total == 6);

  tuple::for_each([](auto& arg) { arg = arg * arg; }, nums);
  CHECK(nums == std::tuple{1, 4, 9});
}

TEST_CASE("for_each multiple arguments") {
  std::tuple u(1, 2, 3);
  std::tuple v(5, 2, -3);

  int dot_product = 0;
  tuple::for_each([&dot_product](int u, int v) { dot_product += u * v; }, u, v);
  CHECK(dot_product == 1 * 5 + 2 * 2 + 3 * -3);
}

TEST_CASE("transform single argument") {
  CHECK(tuple::transform([](auto arg) { return arg * arg; },
                         std::tuple(1, 2, 3)) == std::tuple{1, 4, 9});

  CHECK(tuple::transform([](auto arg) { return arg + arg; },
                         std::tuple(1, std::string{"hello"}, 3)) ==
        std::tuple{2, "hellohello", 6});
}

TEST_CASE("transform multiple arguments") {
  CHECK(tuple::transform([](int num, char c) { return std::string(num, c); },
                         std::tuple(1, 2, 3), std::tuple('x', 'y', 'z')) ==
        std::tuple{"x", "yy", "zzz"});
}

}  // namespace
}  // namespace base
