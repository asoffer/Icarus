#include "core/fn_args.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(FnArgsTest, Size) {
  auto args = FnArgs<int>{};
  EXPECT_EQ(args.size(), 0);
  EXPECT_TRUE(args.empty());

  args = FnArgs<int>({}, {{"hello", -3}, {"world", -5}});
  EXPECT_EQ(args.size(), 2);
  EXPECT_FALSE(args.empty());

  args = FnArgs<int>({1, 4, 9}, {});
  EXPECT_EQ(args.size(), 3);
  EXPECT_FALSE(args.empty());

  args = FnArgs<int>({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  EXPECT_EQ(args.size(), 5);
  EXPECT_FALSE(args.empty());
}

TEST(FnArgsTest, Iterators) {
  auto args = FnArgs<int>{};
  EXPECT_EQ(args.begin(), args.end());

  args = FnArgs<int>({}, {{"hello", -3}, {"world", -5}});
  EXPECT_TRUE(args.begin() != args.end());
}

TEST(FnArgsTest, MutableAccess) {
  FnArgs<int> args({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  EXPECT_EQ(args.pos(), (std::vector{1, 4, 9}));
  EXPECT_THAT(args.named(),
              UnorderedElementsAre(Pair("hello", -3), Pair("world", -5)));
  EXPECT_EQ(args.at_or_null("world!"), nullptr);
}

TEST(FnArgsTest, ConstAccess) {
  FnArgs<int> const args({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  EXPECT_EQ(args.pos(), (std::vector{1, 4, 9}));
  EXPECT_THAT(args.named(),
              UnorderedElementsAre(Pair("hello", -3), Pair("world", -5)));
  EXPECT_EQ(args.at_or_null("world!"), nullptr);
}

TEST(FnArgsTest, Transform) {
  auto squared = FnArgs<int>({1, 2, 3}, {{"hello", -3}, {"world", -5}})
                     .Transform([](int n) { return n * n; });
  EXPECT_EQ(squared.pos(), (std::vector{1, 4, 9}));
  EXPECT_THAT(squared.named(),
              UnorderedElementsAre(Pair("hello", 9), Pair("world", 25)));
}

TEST(FnArgsTest, Apply) {
  int total = 0;
  FnArgs<int>({1, 2, 3}, {{"hello", -3}, {"world", -5}}).Apply([&total](int n) {
    total += n;
  });
  EXPECT_EQ(total, -2);
}

TEST(FnArgsTest, ApplyWithIndex) {
  size_t total = 0;
  FnArgs<size_t>({6, 12, 18}, {{"hello", 10}, {"world", 20}})
      .ApplyWithIndex([&total](auto &&index, size_t n) {
        if constexpr (std::is_same_v<size_t, std::decay_t<decltype(index)>>) {
          total += index;
        } else {
          total += n;
        }
      });
  EXPECT_EQ(total, 33u);
}

TEST(FnArgsTest, ConstIterator) {
  {  // generic
    FnArgs<int> fnargs({1, 2, 3}, {{"hello", -3}, {"world", -5}});
    int total = 0;
    for (int n : fnargs) { total += n; }
    EXPECT_EQ(total, -2);
  }

  {  // positional only
    FnArgs<int> fnargs({1, 2, 3}, {});
    int total = 0;
    for (int n : fnargs) { total += n; }
    EXPECT_EQ(total, 6);
  }

  {  // named only
    FnArgs<int> fnargs({}, {{"hello", -3}, {"world", -5}});
    int total = 0;
    for (int n : fnargs) { total += n; }
    EXPECT_EQ(total, -8);
  }

  {  // empty
    FnArgs<int> fnargs({}, {});
    int total = 0;
    for (int n : fnargs) { total += n; }
    EXPECT_EQ(total, 0);
  }
}

}  // namespace
}  // namespace core
