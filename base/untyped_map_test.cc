#include "base/untyped_map.h"

#include "test/catch.h"

namespace base {
namespace {

TEST_CASE("Default constructor invariants") {
  untyped_map<int> map;
  CHECK(map.size() == 0);
  CHECK(map.empty());
}

TEST_CASE("Emplace") {
  untyped_map<int> map;
  map.emplace(10, true);
  map.emplace(3, 5);
  map.emplace(4, false);
  CHECK(map.get<bool>(10));
  CHECK(map.get<int>(3) == 5);
  CHECK_FALSE(map.get<bool>(4));
  CHECK_FALSE(map.empty());
  CHECK(map.size() == 3);
}

TEST_CASE("Iterate") {
  untyped_map<int> map;
  for (int i = 0; i < 10; ++i) { map.emplace(i, i); }
  int total = 0;
  for (auto [k, v] : map) { total += *reinterpret_cast<int*>(v); }
  CHECK(total == 45);
}

}  // namespace
}  // namespace base
