#include "core/arguments.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace core {
namespace {
using ::testing::Pair;
using ::testing::UnorderedElementsAre;

TEST(ArgumentsTest, Size) {
  auto args = Arguments<int>{};
  EXPECT_EQ(args.size(), 0);
  EXPECT_TRUE(args.empty());

  args = Arguments<int>({}, {{"hello", -3}, {"world", -5}});
  EXPECT_EQ(args.size(), 2);
  EXPECT_FALSE(args.empty());

  args = Arguments<int>({1, 4, 9}, {});
  EXPECT_EQ(args.size(), 3);
  EXPECT_FALSE(args.empty());

  args = Arguments<int>({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  EXPECT_EQ(args.size(), 5);
  EXPECT_FALSE(args.empty());
}

TEST(ArgumentsTest, Iterators) {
  auto args = Arguments<int>{};
  EXPECT_EQ(args.begin(), args.end());

  args = Arguments<int>({}, {{"hello", -3}, {"world", -5}});
  EXPECT_TRUE(args.begin() != args.end());
}

TEST(ArgumentsTest, MutableAccess) {
  Arguments<int> args({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  EXPECT_EQ(args.pos(), (std::vector{1, 4, 9}));
  EXPECT_THAT(args.named(),
              UnorderedElementsAre(Pair("hello", -3), Pair("world", -5)));
  EXPECT_EQ(args.at_or_null("world!"), nullptr);
}

TEST(ArgumentsTest, ConstAccess) {
  Arguments<int> const args({1, 4, 9}, {{"hello", -3}, {"world", -5}});
  EXPECT_EQ(args.pos(), (std::vector{1, 4, 9}));
  EXPECT_THAT(args.named(),
              UnorderedElementsAre(Pair("hello", -3), Pair("world", -5)));
  EXPECT_EQ(args.at_or_null("world!"), nullptr);
}

TEST(ArgumentsTest, Transform) {
  auto squared = Arguments<int>({1, 2, 3}, {{"hello", -3}, {"world", -5}})
                     .Transform([](int n) { return n * n; });
  EXPECT_EQ(squared.pos(), (std::vector{1, 4, 9}));
  EXPECT_THAT(squared.named(),
              UnorderedElementsAre(Pair("hello", 9), Pair("world", 25)));
}

TEST(ArgumentsTest, Apply) {
  int total = 0;
  Arguments<int>({1, 2, 3}, {{"hello", -3}, {"world", -5}}).Apply([&total](int n) {
    total += n;
  });
  EXPECT_EQ(total, -2);
}

TEST(ArgumentsTest, ApplyWithIndex) {
  size_t total = 0;
  Arguments<size_t>({6, 12, 18}, {{"hello", 10}, {"world", 20}})
      .ApplyWithIndex([&total](auto &&index, size_t n) {
        if constexpr (std::is_same_v<size_t, std::decay_t<decltype(index)>>) {
          total += index;
        } else {
          total += n;
        }
      });
  EXPECT_EQ(total, 33u);
}

TEST(ArgumentsTest, ConstIterator) {
  {  // generic
    Arguments<int> fnargs({1, 2, 3}, {{"hello", -3}, {"world", -5}});
    int total = 0;
    for (int n : fnargs) { total += n; }
    EXPECT_EQ(total, -2);
  }

  {  // positional only
    Arguments<int> fnargs({1, 2, 3}, {});
    int total = 0;
    for (int n : fnargs) { total += n; }
    EXPECT_EQ(total, 6);
  }

  {  // named only
    Arguments<int> fnargs({}, {{"hello", -3}, {"world", -5}});
    int total = 0;
    for (int n : fnargs) { total += n; }
    EXPECT_EQ(total, -8);
  }

  {  // empty
    Arguments<int> fnargs({}, {});
    int total = 0;
    for (int n : fnargs) { total += n; }
    EXPECT_EQ(total, 0);
  }
}

}  // namespace
}  // namespace core
