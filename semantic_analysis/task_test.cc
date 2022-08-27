#include "semantic_analysis/task.h"

#include <functional>

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace semantic_analysis {
namespace {

using ::testing::IsEmpty;
enum Phase { Zero, One };

struct Node : base::Extend<Node>::With<base::AbslHashExtension> {
  int *counter = 0;
  Node *next   = nullptr;
};

using TestScheduler = Scheduler<Node, Phase>;

Task<Node, Phase> OneTaskWithNoAwaits(TestScheduler &, Node n) {
  ++*n.counter;
  co_return;
}

TEST(Scheduler, OneTaskWithNoAwaits) {
  TestScheduler s(OneTaskWithNoAwaits);
  int n_counter = 0;
  Node n{.counter = &n_counter};
  s.schedule(n);
  EXPECT_EQ(n_counter, 1);
  s.complete();
  EXPECT_EQ(n_counter, 1);
}

Task<Node, Phase> OneTaskWithOneAwait(TestScheduler &s, Node n) {
  ++*n.counter;
  s.set_completed(TaskPhase<Phase::Zero>(n));
  if (n.next) { co_await TaskPhase<Phase::Zero>(*n.next); }
  ++*n.counter;
}

TEST(Scheduler, OneTaskWithOneAwaits) {
  TestScheduler s(OneTaskWithOneAwait);
  int n1_counter = 0;
  Node n1{.counter = &n1_counter};
  int n2_counter = 0;
  Node n2{.counter = &n2_counter, .next = &n1};

  s.schedule(n2);
  EXPECT_EQ(n1_counter, 2);
  EXPECT_EQ(n2_counter, 1);
  s.complete();
  EXPECT_EQ(n1_counter, 2);
  EXPECT_EQ(n2_counter, 2);
}

Task<Node, Phase> SelfReferential(TestScheduler &s, Node n) {
  ++*n.counter;
  s.set_completed(TaskPhase<Phase::Zero>(n));
  co_await TaskPhase<Phase::Zero>(*n.next);
  ++*n.counter;
  s.set_completed(TaskPhase<Phase::One>(n));
  co_await TaskPhase<Phase::One>(*n.next);
  ++*n.counter;
}

TEST(Scheduler, SelfReferential) {
  TestScheduler s(SelfReferential);
  int n1_counter = 0;
  Node n1{.counter = &n1_counter};
  int n2_counter = 0;
  Node n2{.counter = &n2_counter};
  n1.next = &n2;
  n2.next = &n1;

  s.schedule(n1);
  s.complete();
  EXPECT_EQ(n1_counter, 3);
  EXPECT_EQ(n2_counter, 3);
}

}  // namespace
}  // namespace semantic_analysis
