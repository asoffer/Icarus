#include "base/move_func.h"
#include "test/catch.h"

TEST_CASE("move_func") {
  base::move_func<int(int)> f = [](int n) { return n * n; };
  CHECK(std::move(f)(3) == 9);
  CHECK(f == nullptr);
}

TEST_CASE("cast to bool") {
  base::move_func<int(int)> f = [](int n) { return n * n; };
  CHECK(f);
  auto g = std::move(f);
  CHECK_FALSE(f);
  CHECK(g);
}
