#include "core/cycle_tracker.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {

using ::testing::_;
using ::testing::ElementsAre;

TEST(CycleTracker, NoErrors) {
  CycleTracker<int> tracker;

  testing::MockFunction<void(absl::Span<int const>)> function;
  EXPECT_CALL(function, Call(_)).Times(0);

  tracker.push(1, function.AsStdFunction());
  tracker.push(2, function.AsStdFunction());
  tracker.push(3, function.AsStdFunction());
  tracker.pop();
  tracker.pop();
  tracker.push(3, function.AsStdFunction());
}

TEST(CycleTracker, Cycle) {
  CycleTracker<int> tracker;

  testing::MockFunction<void(absl::Span<int const>)> function;
  EXPECT_CALL(function, Call(ElementsAre(2, 3))).Times(1);

  tracker.push(1, function.AsStdFunction());
  tracker.push(2, function.AsStdFunction());
  tracker.push(3, function.AsStdFunction());
  tracker.push(2, function.AsStdFunction());

  EXPECT_FALSE(tracker.has_error(1));
  EXPECT_TRUE(tracker.has_error(2));
  EXPECT_TRUE(tracker.has_error(3));
}

}  // namespace
}  // namespace core
