#include "semantic_analysis/task.h"

#include <functional>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "nth/meta/type.h"

namespace semantic_analysis {
namespace {

using ::testing::IsEmpty;
enum PhaseId { Zero, One, Completed };

struct Node : base::Extend<Node>::With<base::AbslHashExtension> {
  int *counter = 0;
  Node *next   = nullptr;
};

using NonReturningTask =
    Task<Node, PhaseId, nth::type_sequence<void(), void(), void()>>;
using TestScheduler = Scheduler<NonReturningTask>;

NonReturningTask OneTaskWithNoAwaits(TestScheduler &, Node n) {
  ++*n.counter;
  co_return YieldResult<NonReturningTask, PhaseId::Zero>(n);
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

NonReturningTask OneTaskWithOneAwait(TestScheduler &, Node n) {
  ++*n.counter;
  co_yield YieldResult<NonReturningTask, PhaseId::Zero>(n);
  if (n.next) { co_await Phase<NonReturningTask, PhaseId::Zero>(*n.next); }
  ++*n.counter;
  co_return YieldResult<NonReturningTask, PhaseId::One>(n);
}

TEST(Scheduler, OneTaskWithOneAwait) {
  TestScheduler s(OneTaskWithOneAwait);
  int n1_counter = 0;
  Node n1{.counter = &n1_counter};
  int n2_counter = 0;
  Node n2{.counter = &n2_counter, .next = &n1};

  s.schedule(n2);
  EXPECT_EQ(n1_counter, 2);
  EXPECT_EQ(n2_counter, 2);
  s.complete();
  EXPECT_EQ(n1_counter, 2);
  EXPECT_EQ(n2_counter, 2);
}

NonReturningTask SelfReferential(TestScheduler &, Node n) {
  ++*n.counter;
  co_yield YieldResult<NonReturningTask, PhaseId::Zero>(n);
  co_await Phase<NonReturningTask, PhaseId::Zero>(*n.next);
  ++*n.counter;
  co_yield YieldResult<NonReturningTask, PhaseId::One>(n);
  co_await Phase<NonReturningTask, PhaseId::One>(*n.next);
  ++*n.counter;
  co_return YieldResult<NonReturningTask, PhaseId::Completed>(n);
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

using ReturningTask =
    Task<Node, PhaseId, nth::type_sequence<int(), std::string_view(), void()>>;
using ReturnTestScheduler = Scheduler<ReturningTask>;

ReturningTask ReturningSelfReferential(ReturnTestScheduler &, Node n) {
  ++*n.counter;
  co_yield YieldResult<ReturningTask, PhaseId::Zero>(n, 3);
  *n.counter += co_await Phase<ReturningTask, PhaseId::Zero>(*n.next);
  ++*n.counter;
  co_yield YieldResult<ReturningTask, PhaseId::One>(n, "hello");
  *n.counter += (co_await Phase<ReturningTask, PhaseId::One>(*n.next)).size();
  ++*n.counter;
  co_return YieldResult<ReturningTask, PhaseId::Completed>(n);
}

TEST(Scheduler, ReturningSelfReferential) {
  ReturnTestScheduler s(ReturningSelfReferential);
  int n1_counter = 0;
  Node n1{.counter = &n1_counter};
  int n2_counter = 0;
  Node n2{.counter = &n2_counter};
  n1.next = &n2;
  n2.next = &n1;

  s.schedule(n1);
  s.complete();
  EXPECT_EQ(n1_counter, 11);
  EXPECT_EQ(n2_counter, 11);
}

ReturningTask SkipPhaseZero(ReturnTestScheduler &, Node n) {
  ++*n.counter;
  co_yield YieldResult<ReturningTask, PhaseId::One>(n, "hello");
  // When a phase is skipped over, the returned value should be the
  // value-initialized value of the returned type.
  *n.counter += co_await Phase<ReturningTask, PhaseId::Zero>(*n.next);
  *n.counter += (co_await Phase<ReturningTask, PhaseId::One>(*n.next)).size();
  co_return YieldResult<ReturningTask, PhaseId::Completed>(n);
}

TEST(Scheduler, SkipPhaseZero) {
  ReturnTestScheduler s(SkipPhaseZero);
  int n1_counter = 0;
  Node n1{.counter = &n1_counter};
  int n2_counter = 0;
  Node n2{.counter = &n2_counter};
  n1.next = &n2;
  n2.next = &n1;

  s.schedule(n1);
  s.complete();
  EXPECT_EQ(n1_counter, 6);
  EXPECT_EQ(n2_counter, 6);
}

}  // namespace
}  // namespace semantic_analysis
