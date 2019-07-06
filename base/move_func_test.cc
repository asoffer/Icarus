#include "base/move_func.h"
#include "test/catch.h"

TEST_CASE("move_func") {
  base::move_func<int(int)> f = [](int n) { return n * n; };
  CHECK(std::move(f)(3) == 9);
  CHECK(f == nullptr);
}
