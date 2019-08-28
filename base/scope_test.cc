#include "base/scope.h"
#include "test/catch.h"

#include <type_traits>

namespace base {
namespace {

int depth = 0;

struct DepthScope : public base::UseWithScope {
  DepthScope() { ++depth; }
  ~DepthScope() { --depth; }
};

TEST_CASE("depth") {
  CHECK(depth == 0);
  ICARUS_SCOPE(DepthScope()) {
    CHECK(depth == 1);
    ICARUS_SCOPE(DepthScope()) { CHECK(depth == 2); }
    CHECK(depth == 1);
    ICARUS_SCOPE(DepthScope()) { CHECK(depth == 2); }
    CHECK(depth == 1);
  }
  CHECK(depth == 0);
}

}  // namespace
}  // namespace base
