#include "base/no_destructor.h"

#include "gtest/gtest.h"

namespace {

struct DtorCounter {
  explicit DtorCounter(int *count) : count_(count) {}
  ~DtorCounter() { ++*count_; }

 private:
  int *count_;
};

TEST(NoDestructor, NotDestroyed) {
  int dtor_count = 0;
  {
    base::NoDestructor<DtorCounter> counter(&dtor_count);
    EXPECT_EQ(dtor_count, 0);
  }
  EXPECT_EQ(dtor_count, 0);
}

}  // namespace
