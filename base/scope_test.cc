#include "base/scope.h"

#include <type_traits>

#include "gtest/gtest.h"

namespace base {
namespace {

int depth = 0;

struct DepthScope : public base::UseWithScope {
  DepthScope() { ++depth; }
  ~DepthScope() { --depth; }
};

TEST(Scope, Depth) {
  EXPECT_EQ(depth, 0);
  ICARUS_SCOPE(DepthScope()) {
    EXPECT_EQ(depth, 1);
    ICARUS_SCOPE(DepthScope()) { EXPECT_EQ(depth, 2); }
    EXPECT_EQ(depth, 1);
    ICARUS_SCOPE(DepthScope()) { EXPECT_EQ(depth, 2); }
    EXPECT_EQ(depth, 1);
  }
  EXPECT_EQ(depth, 0);
}

}  // namespace
}  // namespace base
