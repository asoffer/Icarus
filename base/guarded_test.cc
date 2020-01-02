#include "base/guarded.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace {
struct MockMutex {
  void lock() { is_locked_ = true; }
  void unlock() { is_locked_ = false; }

  static bool locked() { return is_locked_; }

 private:
  static bool is_locked_;
};
bool MockMutex::is_locked_ = false;

TEST(Guarded, LockingRValue) {
  ASSERT_FALSE(MockMutex::locked()) << "Test initialization failure";

  base::guarded<int, MockMutex> g;
  EXPECT_FALSE(MockMutex::locked());
  g.lock();
  EXPECT_FALSE(MockMutex::locked());
}


TEST(Guarded, LockingLValue) {
  ASSERT_FALSE(MockMutex::locked()) << "Test initialization failure";

  base::guarded<int, MockMutex> g;
  EXPECT_FALSE(MockMutex::locked());
  {
    auto handle = g.lock();
    EXPECT_TRUE(MockMutex::locked());
  }
  EXPECT_FALSE(MockMutex::locked());
}

TEST(Guarded, LockingCommaOperator) {
  ASSERT_FALSE(MockMutex::locked()) << "Test initialization failure";

  base::guarded<int, MockMutex> g;
  EXPECT_FALSE(MockMutex::locked());

  bool was_locked = (g.lock(), MockMutex::locked());

  EXPECT_TRUE(was_locked);
  EXPECT_FALSE(MockMutex::locked());
}

}  // namespace
