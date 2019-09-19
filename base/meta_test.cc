#include "base/meta.h"
#include "test/catch.h"

#include <type_traits>

namespace base {
namespace {

TEST_CASE("first") {
  CHECK(std::is_same_v<first_t<int>, int>);
  CHECK(std::is_same_v<first_t<int, int>, int>);
  CHECK(std::is_same_v<first_t<int, bool>, int>);
  CHECK(std::is_same_v<first_t<bool, int>, bool>);
}

}  // namespace
}  // namespace base